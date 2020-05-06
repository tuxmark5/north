// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// The sheer existence of this file is an awful hack. See the comments in
// `src/bootstrap/native.rs` for why this is needed when compiling LLD.

use std::env;
use std::process::{self, Stdio, Command};
use std::io::{self, Write};

fn main() {
    let real_llvm_config = env::var_os("LLVM_CONFIG_REAL").unwrap();
    let mut cmd = Command::new(real_llvm_config);
    cmd.args(env::args().skip(1)).stderr(Stdio::piped());
    let output = cmd.output().expect("failed to spawn llvm-config");
    let stdout = String::from_utf8_lossy(&output.stdout);
    print!("{}", stdout.replace("\\", "/"));
    io::stdout().flush().unwrap();
    process::exit(output.status.code().unwrap_or(1));
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// rustbuild, the Rust build system
//
// This is the entry point for the build system used to compile the `rustc`
// compiler. Lots of documentation can be found in the `README.md` file in the
// parent directory, and otherwise documentation can be found throughout the `build`
// directory in each respective module.

#[deny(warnings)]

extern crate bootstrap;

use std::env;

use bootstrap::{Config, Build};

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    let config = Config::parse(&args);
    Build::new(config).build();
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Shim which is passed to Cargo as "rustc" when running the bootstrap.
//
// This shim will take care of some various tasks that our build process
// requires that Cargo can't quite do through normal configuration:
//
// 1. When compiling build scripts and build dependencies, we need a guaranteed
//    full standard library available. The only compiler which actually has
//    this is the snapshot, so we detect this situation and always compile with
//    the snapshot compiler.
// 2. We pass a bunch of `--cfg` and other flags based on what we're compiling
//    (and this slightly differs based on a whether we're using a snapshot or
//    not), so we do that all here.
//
// This may one day be replaced by RUSTFLAGS, but the dynamic nature of
// switching compilers for the bootstrap and for build scripts will probably
// never get replaced.

#[deny(warnings)]

extern crate bootstrap;

use std::env;
use std::ffi::OsString;
use std::io;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;
use std::time::Instant;

fn main() {
    let mut args = env::args_os().skip(1).collect::<Vec<_>>();

    // Append metadata suffix for internal crates. See the corresponding entry
    // in bootstrap/lib.rs for details.
    if let Ok(s) = env::var("RUSTC_METADATA_SUFFIX") {
        for i in 1..args.len() {
            // Dirty code for borrowing issues
            let mut new = None;
            if let Some(current_as_str) = args[i].to_str() {
                if (&*args[i - 1] == "-C" && current_as_str.starts_with("metadata")) ||
                   current_as_str.starts_with("-Cmetadata") {
                    new = Some(format!("{}-{}", current_as_str, s));
                }
            }
            if let Some(new) = new { args[i] = new.into(); }
        }
    }

    // Drop `--error-format json` because despite our desire for json messages
    // from Cargo we don't want any from rustc itself.
    if let Some(n) = args.iter().position(|n| n == "--error-format") {
        args.remove(n);
        args.remove(n);
    }

    if let Some(s) = env::var_os("RUSTC_ERROR_FORMAT") {
        args.push("--error-format".into());
        args.push(s);
    }

    // Detect whether or not we're a build script depending on whether --target
    // is passed (a bit janky...)
    let target = args.windows(2)
        .find(|w| &*w[0] == "--target")
        .and_then(|w| w[1].to_str());
    let version = args.iter().find(|w| &**w == "-vV");

    let verbose = match env::var("RUSTC_VERBOSE") {
        Ok(s) => usize::from_str(&s).expect("RUSTC_VERBOSE should be an integer"),
        Err(_) => 0,
    };

    // Use a different compiler for build scripts, since there may not yet be a
    // libstd for the real compiler to use. However, if Cargo is attempting to
    // determine the version of the compiler, the real compiler needs to be
    // used. Currently, these two states are differentiated based on whether
    // --target and -vV is/isn't passed.
    let (rustc, libdir) = if target.is_none() && version.is_none() {
        ("RUSTC_SNAPSHOT", "RUSTC_SNAPSHOT_LIBDIR")
    } else {
        ("RUSTC_REAL", "RUSTC_LIBDIR")
    };
    let stage = env::var("RUSTC_STAGE").expect("RUSTC_STAGE was not set");
    let sysroot = env::var_os("RUSTC_SYSROOT").expect("RUSTC_SYSROOT was not set");
    let on_fail = env::var_os("RUSTC_ON_FAIL").map(|of| Command::new(of));

    let rustc = env::var_os(rustc).unwrap_or_else(|| panic!("{:?} was not set", rustc));
    let libdir = env::var_os(libdir).unwrap_or_else(|| panic!("{:?} was not set", libdir));
    let mut dylib_path = bootstrap::util::dylib_path();
    dylib_path.insert(0, PathBuf::from(&libdir));

    let mut cmd = Command::new(rustc);
    cmd.args(&args)
        .arg("--cfg")
        .arg(format!("stage{}", stage))
        .env(bootstrap::util::dylib_path_var(),
             env::join_paths(&dylib_path).unwrap());
    let mut maybe_crate = None;

    // Print backtrace in case of ICE
    if env::var("RUSTC_BACKTRACE_ON_ICE").is_ok() && env::var("RUST_BACKTRACE").is_err() {
        cmd.env("RUST_BACKTRACE", "1");
    }

    cmd.env("RUSTC_BREAK_ON_ICE", "1");

    if let Some(target) = target {
        // The stage0 compiler has a special sysroot distinct from what we
        // actually downloaded, so we just always pass the `--sysroot` option.
        cmd.arg("--sysroot").arg(&sysroot);

        // When we build Rust dylibs they're all intended for intermediate
        // usage, so make sure we pass the -Cprefer-dynamic flag instead of
        // linking all deps statically into the dylib.
        if env::var_os("RUSTC_NO_PREFER_DYNAMIC").is_none() {
            cmd.arg("-Cprefer-dynamic");
        }

        // Help the libc crate compile by assisting it in finding the MUSL
        // native libraries.
        if let Some(s) = env::var_os("MUSL_ROOT") {
            let mut root = OsString::from("native=");
            root.push(&s);
            root.push("/lib");
            cmd.arg("-L").arg(&root);
        }

        // Override linker if necessary.
        if let Ok(target_linker) = env::var("RUSTC_TARGET_LINKER") {
            cmd.arg(format!("-Clinker={}", target_linker));
        }

        let crate_name = args.windows(2)
            .find(|a| &*a[0] == "--crate-name")
            .unwrap();
        let crate_name = &*crate_name[1];
        maybe_crate = Some(crate_name);

        // If we're compiling specifically the `panic_abort` crate then we pass
        // the `-C panic=abort` option. Note that we do not do this for any
        // other crate intentionally as this is the only crate for now that we
        // ship with panic=abort.
        //
        // This... is a bit of a hack how we detect this. Ideally this
        // information should be encoded in the crate I guess? Would likely
        // require an RFC amendment to RFC 1513, however.
        //
        // `compiler_builtins` are unconditionally compiled with panic=abort to
        // workaround undefined references to `rust_eh_unwind_resume` generated
        // otherwise, see issue https://github.com/rust-lang/rust/issues/43095.
        if crate_name == "panic_abort" ||
           crate_name == "compiler_builtins" && stage != "0" {
            cmd.arg("-C").arg("panic=abort");
        }

        // Set various options from config.toml to configure how we're building
        // code.
        if env::var("RUSTC_DEBUGINFO") == Ok("true".to_string()) {
            cmd.arg("-g");
        } else if env::var("RUSTC_DEBUGINFO_LINES") == Ok("true".to_string()) {
            cmd.arg("-Cdebuginfo=1");
        }
        let debug_assertions = match env::var("RUSTC_DEBUG_ASSERTIONS") {
            Ok(s) => if s == "true" { "y" } else { "n" },
            Err(..) => "n",
        };

        // The compiler builtins are pretty sensitive to symbols referenced in
        // libcore and such, so we never compile them with debug assertions.
        if crate_name == "compiler_builtins" {
            cmd.arg("-C").arg("debug-assertions=no");
        } else {
            cmd.arg("-C").arg(format!("debug-assertions={}", debug_assertions));
        }

        if let Ok(s) = env::var("RUSTC_CODEGEN_UNITS") {
            cmd.arg("-C").arg(format!("codegen-units={}", s));
        }

        // Emit save-analysis info.
        if env::var("RUSTC_SAVE_ANALYSIS") == Ok("api".to_string()) {
            cmd.arg("-Zsave-analysis");
            cmd.env("RUST_SAVE_ANALYSIS_CONFIG",
                    "{\"output_file\": null,\"full_docs\": false,\
                     \"pub_only\": true,\"reachable_only\": false,\
                     \"distro_crate\": true,\"signatures\": false,\"borrow_data\": false}");
        }

        // Dealing with rpath here is a little special, so let's go into some
        // detail. First off, `-rpath` is a linker option on Unix platforms
        // which adds to the runtime dynamic loader path when looking for
        // dynamic libraries. We use this by default on Unix platforms to ensure
        // that our nightlies behave the same on Windows, that is they work out
        // of the box. This can be disabled, of course, but basically that's why
        // we're gated on RUSTC_RPATH here.
        //
        // Ok, so the astute might be wondering "why isn't `-C rpath` used
        // here?" and that is indeed a good question to task. This codegen
        // option is the compiler's current interface to generating an rpath.
        // Unfortunately it doesn't quite suffice for us. The flag currently
        // takes no value as an argument, so the compiler calculates what it
        // should pass to the linker as `-rpath`. This unfortunately is based on
        // the **compile time** directory structure which when building with
        // Cargo will be very different than the runtime directory structure.
        //
        // All that's a really long winded way of saying that if we use
        // `-Crpath` then the executables generated have the wrong rpath of
        // something like `$ORIGIN/deps` when in fact the way we distribute
        // rustc requires the rpath to be `$ORIGIN/../lib`.
        //
        // So, all in all, to set up the correct rpath we pass the linker
        // argument manually via `-C link-args=-Wl,-rpath,...`. Plus isn't it
        // fun to pass a flag to a tool to pass a flag to pass a flag to a tool
        // to change a flag in a binary?
        if env::var("RUSTC_RPATH") == Ok("true".to_string()) {
            let rpath = if target.contains("apple") {

                // Note that we need to take one extra step on macOS to also pass
                // `-Wl,-instal_name,@rpath/...` to get things to work right. To
                // do that we pass a weird flag to the compiler to get it to do
                // so. Note that this is definitely a hack, and we should likely
                // flesh out rpath support more fully in the future.
                cmd.arg("-Z").arg("osx-rpath-install-name");
                Some("-Wl,-rpath,@loader_path/../lib")
            } else if !target.contains("windows") && !target.contains("wasm32") {
                Some("-Wl,-rpath,$ORIGIN/../lib")
            } else {
                None
            };
            if let Some(rpath) = rpath {
                cmd.arg("-C").arg(format!("link-args={}", rpath));
            }
        }

        if let Ok(s) = env::var("RUSTC_CRT_STATIC") {
            if s == "true" {
                cmd.arg("-C").arg("target-feature=+crt-static");
            }
            if s == "false" {
                cmd.arg("-C").arg("target-feature=-crt-static");
            }
        }

        // When running miri tests, we need to generate MIR for all libraries
        if env::var("TEST_MIRI").ok().map_or(false, |val| val == "true") {
            cmd.arg("-Zalways-encode-mir");
            cmd.arg("-Zmir-emit-validate=1");
        }

        // Force all crates compiled by this compiler to (a) be unstable and (b)
        // allow the `rustc_private` feature to link to other unstable crates
        // also in the sysroot.
        if env::var_os("RUSTC_FORCE_UNSTABLE").is_some() {
            cmd.arg("-Z").arg("force-unstable-if-unmarked");
        }
    } else {
        // Override linker if necessary.
        if let Ok(host_linker) = env::var("RUSTC_HOST_LINKER") {
            cmd.arg(format!("-Clinker={}", host_linker));
        }

        if let Ok(s) = env::var("RUSTC_HOST_CRT_STATIC") {
            if s == "true" {
                cmd.arg("-C").arg("target-feature=+crt-static");
            }
            if s == "false" {
                cmd.arg("-C").arg("target-feature=-crt-static");
            }
        }
    }

    if env::var_os("RUSTC_PARALLEL_QUERIES").is_some() {
        cmd.arg("--cfg").arg("parallel_queries");
    }

    if env::var_os("RUSTC_VERIFY_LLVM_IR").is_some() {
        cmd.arg("-Z").arg("verify-llvm-ir");
    }

    let color = match env::var("RUSTC_COLOR") {
        Ok(s) => usize::from_str(&s).expect("RUSTC_COLOR should be an integer"),
        Err(_) => 0,
    };

    if color != 0 {
        cmd.arg("--color=always");
    }

    if env::var_os("RUSTC_DENY_WARNINGS").is_some() && env::var_os("RUSTC_EXTERNAL_TOOL").is_none()
    {
        cmd.arg("-Dwarnings");
        cmd.arg("-Dbare_trait_objects");
    }

    if verbose > 1 {
        eprintln!(
            "rustc command: {:?}={:?} {:?}",
            bootstrap::util::dylib_path_var(),
            env::join_paths(&dylib_path).unwrap(),
            cmd,
        );
        eprintln!("sysroot: {:?}", sysroot);
        eprintln!("libdir: {:?}", libdir);
    }

    if let Some(mut on_fail) = on_fail {
        let e = match cmd.status() {
            Ok(s) if s.success() => std::process::exit(0),
            e => e,
        };
        println!("\nDid not run successfully: {:?}\n{:?}\n-------------", e, cmd);
        exec_cmd(&mut on_fail).expect("could not run the backup command");
        std::process::exit(1);
    }

    if env::var_os("RUSTC_PRINT_STEP_TIMINGS").is_some() {
        if let Some(krate) = maybe_crate {
            let start = Instant::now();
            let status = cmd
                .status()
                .unwrap_or_else(|_| panic!("\n\n failed to run {:?}", cmd));
            let dur = start.elapsed();

            let is_test = args.iter().any(|a| a == "--test");
            eprintln!("[RUSTC-TIMING] {} test:{} {}.{:03}",
                      krate.to_string_lossy(),
                      is_test,
                      dur.as_secs(),
                      dur.subsec_nanos() / 1_000_000);

            match status.code() {
                Some(i) => std::process::exit(i),
                None => {
                    eprintln!("rustc exited with {}", status);
                    std::process::exit(0xfe);
                }
            }
        }
    }

    let code = exec_cmd(&mut cmd).unwrap_or_else(|_| panic!("\n\n failed to run {:?}", cmd));
    std::process::exit(code);
}

#[cfg(unix)]
fn exec_cmd(cmd: &mut Command) -> io::Result<i32> {
    use std::os::unix::process::CommandExt;
    Err(cmd.exec())
}

#[cfg(not(unix))]
fn exec_cmd(cmd: &mut Command) -> io::Result<i32> {
    cmd.status().map(|status| status.code().unwrap())
}
// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Shim which is passed to Cargo as "rustdoc" when running the bootstrap.
//
// See comments in `src/bootstrap/rustc.rs` for more information.

#[deny(warnings)]

extern crate bootstrap;

use std::env;
use std::process::Command;
use std::path::PathBuf;

fn main() {
    let args = env::args_os().skip(1).collect::<Vec<_>>();
    let rustdoc = env::var_os("RUSTDOC_REAL").expect("RUSTDOC_REAL was not set");
    let libdir = env::var_os("RUSTDOC_LIBDIR").expect("RUSTDOC_LIBDIR was not set");
    let stage = env::var("RUSTC_STAGE").expect("RUSTC_STAGE was not set");
    let sysroot = env::var_os("RUSTC_SYSROOT").expect("RUSTC_SYSROOT was not set");

    use std::str::FromStr;

    let verbose = match env::var("RUSTC_VERBOSE") {
        Ok(s) => usize::from_str(&s).expect("RUSTC_VERBOSE should be an integer"),
        Err(_) => 0,
    };

    let mut dylib_path = bootstrap::util::dylib_path();
    dylib_path.insert(0, PathBuf::from(libdir.clone()));

    //FIXME(misdreavus): once stdsimd uses cfg(rustdoc) instead of cfg(dox), remove the `--cfg dox`
    //arguments here
    let mut cmd = Command::new(rustdoc);
    cmd.args(&args)
        .arg("--cfg")
        .arg(format!("stage{}", stage))
        .arg("--cfg")
        .arg("dox")
        .arg("--sysroot")
        .arg(sysroot)
        .env(bootstrap::util::dylib_path_var(),
             env::join_paths(&dylib_path).unwrap());

    // Force all crates compiled by this compiler to (a) be unstable and (b)
    // allow the `rustc_private` feature to link to other unstable crates
    // also in the sysroot.
    if env::var_os("RUSTC_FORCE_UNSTABLE").is_some() {
        cmd.arg("-Z").arg("force-unstable-if-unmarked");
    }
    if let Some(linker) = env::var_os("RUSTC_TARGET_LINKER") {
        cmd.arg("--linker").arg(linker).arg("-Z").arg("unstable-options");
    }

    // Bootstrap's Cargo-command builder sets this variable to the current Rust version; let's pick
    // it up so we can make rustdoc print this into the docs
    if let Some(version) = env::var_os("RUSTDOC_CRATE_VERSION") {
        // This "unstable-options" can be removed when `--crate-version` is stabilized
        cmd.arg("-Z")
           .arg("unstable-options")
           .arg("--crate-version").arg(version);
    }

    if verbose > 1 {
        eprintln!("rustdoc command: {:?}", cmd);
        eprintln!("libdir: {:?}", libdir);
    }

    std::process::exit(match cmd.status() {
        Ok(s) => s.code().unwrap_or(1),
        Err(e) => panic!("\n\nfailed to run {:?}: {}\n\n", cmd, e),
    })
}
// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

extern crate cc;

use std::env;
use std::process::{self, Command};

fn main() {
    let target = env::var("SCCACHE_TARGET").unwrap();
    // Locate the actual compiler that we're invoking
    env::set_var("CC", env::var_os("SCCACHE_CC").unwrap());
    env::set_var("CXX", env::var_os("SCCACHE_CXX").unwrap());
    let mut cfg = cc::Build::new();
    cfg.cargo_metadata(false)
       .out_dir("/")
       .target(&target)
       .host(&target)
       .opt_level(0)
       .warnings(false)
       .debug(false);
    let compiler = cfg.get_compiler();

    // Invoke sccache with said compiler
    let sccache_path = env::var_os("SCCACHE_PATH").unwrap();
    let mut cmd = Command::new(&sccache_path);
    cmd.arg(compiler.path());
    for &(ref k, ref v) in compiler.env() {
        cmd.env(k, v);
    }
    for arg in env::args().skip(1) {
        cmd.arg(arg);
    }

    if let Ok(s) = env::var("SCCACHE_EXTRA_ARGS") {
        for s in s.split_whitespace() {
            cmd.arg(s);
        }
    }

    let status = cmd.status().expect("failed to spawn");
    process::exit(status.code().unwrap_or(2))
}
// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::any::Any;
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::fs;
use std::hash::Hash;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use cache::{Cache, Interned, INTERNER};
use check;
use compile;
use dist;
use doc;
use flags::Subcommand;
use install;
use native;
use test;
use tool;
use util::{add_lib_path, exe, libdir};
use {Build, DocTests, Mode};

pub use Compiler;

use petgraph::graph::NodeIndex;
use petgraph::Graph;

pub struct Builder<'a> {
    pub build: &'a Build,
    pub top_stage: u32,
    pub kind: Kind,
    cache: Cache,
    stack: RefCell<Vec<Box<dyn Any>>>,
    time_spent_on_dependencies: Cell<Duration>,
    pub paths: Vec<PathBuf>,
    graph_nodes: RefCell<HashMap<String, NodeIndex>>,
    graph: RefCell<Graph<String, bool>>,
    parent: Cell<Option<NodeIndex>>,
}

impl<'a> Deref for Builder<'a> {
    type Target = Build;

    fn deref(&self) -> &Self::Target {
        self.build
    }
}

pub trait Step: 'static + Clone + Debug + PartialEq + Eq + Hash {
    /// `PathBuf` when directories are created or to return a `Compiler` once
    /// it's been assembled.
    type Output: Clone;

    const DEFAULT: bool = false;

    /// Run this rule for all hosts without cross compiling.
    const ONLY_HOSTS: bool = false;

    /// Primary function to execute this rule. Can call `builder.ensure(...)`
    /// with other steps to run those.
    fn run(self, builder: &Builder) -> Self::Output;

    /// When bootstrap is passed a set of paths, this controls whether this rule
    /// will execute. However, it does not get called in a "default" context
    /// when we are not passed any paths; in that case, make_run is called
    /// directly.
    fn should_run(run: ShouldRun) -> ShouldRun;

    /// Build up a "root" rule, either as a default rule or from a path passed
    /// to us.
    ///
    /// When path is `None`, we are executing in a context where no paths were
    /// passed. When `./x.py build` is run, for example, this rule could get
    /// called if it is in the correct list below with a path of `None`.
    fn make_run(_run: RunConfig) {
        // It is reasonable to not have an implementation of make_run for rules
        // who do not want to get called from the root context. This means that
        // they are likely dependencies (e.g., sysroot creation) or similar, and
        // as such calling them from ./x.py isn't logical.
        unimplemented!()
    }
}

pub struct RunConfig<'a> {
    pub builder: &'a Builder<'a>,
    pub host: Interned<String>,
    pub target: Interned<String>,
    pub path: PathBuf,
}

struct StepDescription {
    default: bool,
    only_hosts: bool,
    should_run: fn(ShouldRun) -> ShouldRun,
    make_run: fn(RunConfig),
    name: &'static str,
}

#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq)]
pub enum PathSet {
    Set(BTreeSet<PathBuf>),
    Suite(PathBuf),
}

impl PathSet {
    fn empty() -> PathSet {
        PathSet::Set(BTreeSet::new())
    }

    fn one<P: Into<PathBuf>>(path: P) -> PathSet {
        let mut set = BTreeSet::new();
        set.insert(path.into());
        PathSet::Set(set)
    }

    fn has(&self, needle: &Path) -> bool {
        match self {
            PathSet::Set(set) => set.iter().any(|p| p.ends_with(needle)),
            PathSet::Suite(_) => false,
        }
    }

    fn path(&self, builder: &Builder) -> PathBuf {
        match self {
            PathSet::Set(set) => set
                .iter()
                .next()
                .unwrap_or(&builder.build.src)
                .to_path_buf(),
            PathSet::Suite(path) => PathBuf::from(path),
        }
    }
}

impl StepDescription {
    fn from<S: Step>() -> StepDescription {
        StepDescription {
            default: S::DEFAULT,
            only_hosts: S::ONLY_HOSTS,
            should_run: S::should_run,
            make_run: S::make_run,
            name: unsafe { ::std::intrinsics::type_name::<S>() },
        }
    }

    fn maybe_run(&self, builder: &Builder, pathset: &PathSet) {
        if builder.config.exclude.iter().any(|e| pathset.has(e)) {
            eprintln!("Skipping {:?} because it is excluded", pathset);
            return;
        } else if !builder.config.exclude.is_empty() {
            eprintln!(
                "{:?} not skipped for {:?} -- not in {:?}",
                pathset, self.name, builder.config.exclude
            );
        }
        let hosts = &builder.hosts;

        // Determine the targets participating in this rule.
        let targets = if self.only_hosts {
            if !builder.config.run_host_only {
                return; // don't run anything
            } else {
                &builder.hosts
            }
        } else {
            &builder.targets
        };

        for host in hosts {
            for target in targets {
                let run = RunConfig {
                    builder,
                    path: pathset.path(builder),
                    host: *host,
                    target: *target,
                };
                (self.make_run)(run);
            }
        }
    }

    fn run(v: &[StepDescription], builder: &Builder, paths: &[PathBuf]) {
        let should_runs = v
            .iter()
            .map(|desc| (desc.should_run)(ShouldRun::new(builder)))
            .collect::<Vec<_>>();

        // sanity checks on rules
        for (desc, should_run) in v.iter().zip(&should_runs) {
            assert!(
                !should_run.paths.is_empty(),
                "{:?} should have at least one pathset",
                desc.name
            );
        }

        if paths.is_empty() {
            for (desc, should_run) in v.iter().zip(should_runs) {
                if desc.default && should_run.is_really_default {
                    for pathset in &should_run.paths {
                        desc.maybe_run(builder, pathset);
                    }
                }
            }
        } else {
            for path in paths {
                // strip CurDir prefix if present
                let path = match path.strip_prefix(".") {
                    Ok(p) => p,
                    Err(_) => path,
                };

                let mut attempted_run = false;
                for (desc, should_run) in v.iter().zip(&should_runs) {
                    if let Some(suite) = should_run.is_suite_path(path) {
                        attempted_run = true;
                        desc.maybe_run(builder, suite);
                    } else if let Some(pathset) = should_run.pathset_for_path(path) {
                        attempted_run = true;
                        desc.maybe_run(builder, pathset);
                    }
                }

                if !attempted_run {
                    panic!("Error: no rules matched {}.", path.display());
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct ShouldRun<'a> {
    pub builder: &'a Builder<'a>,
    // use a BTreeSet to maintain sort order
    paths: BTreeSet<PathSet>,

    // If this is a default rule, this is an additional constraint placed on
    // its run. Generally something like compiler docs being enabled.
    is_really_default: bool,
}

impl<'a> ShouldRun<'a> {
    fn new(builder: &'a Builder) -> ShouldRun<'a> {
        ShouldRun {
            builder,
            paths: BTreeSet::new(),
            is_really_default: true, // by default no additional conditions
        }
    }

    pub fn default_condition(mut self, cond: bool) -> Self {
        self.is_really_default = cond;
        self
    }

    // Unlike `krate` this will create just one pathset. As such, it probably shouldn't actually
    // ever be used, but as we transition to having all rules properly handle passing krate(...) by
    // actually doing something different for every crate passed.
    pub fn all_krates(mut self, name: &str) -> Self {
        let mut set = BTreeSet::new();
        for krate in self.builder.in_tree_crates(name) {
            set.insert(PathBuf::from(&krate.path));
        }
        self.paths.insert(PathSet::Set(set));
        self
    }

    pub fn krate(mut self, name: &str) -> Self {
        for krate in self.builder.in_tree_crates(name) {
            self.paths.insert(PathSet::one(&krate.path));
        }
        self
    }

    // single, non-aliased path
    pub fn path(self, path: &str) -> Self {
        self.paths(&[path])
    }

    // multiple aliases for the same job
    pub fn paths(mut self, paths: &[&str]) -> Self {
        self.paths
            .insert(PathSet::Set(paths.iter().map(PathBuf::from).collect()));
        self
    }

    pub fn is_suite_path(&self, path: &Path) -> Option<&PathSet> {
        self.paths.iter().find(|pathset| match pathset {
            PathSet::Suite(p) => path.starts_with(p),
            PathSet::Set(_) => false,
        })
    }

    pub fn suite_path(mut self, suite: &str) -> Self {
        self.paths.insert(PathSet::Suite(PathBuf::from(suite)));
        self
    }

    // allows being more explicit about why should_run in Step returns the value passed to it
    pub fn never(mut self) -> ShouldRun<'a> {
        self.paths.insert(PathSet::empty());
        self
    }

    fn pathset_for_path(&self, path: &Path) -> Option<&PathSet> {
        self.paths.iter().find(|pathset| pathset.has(path))
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Kind {
    Build,
    Check,
    Test,
    Bench,
    Dist,
    Doc,
    Install,
}

impl<'a> Builder<'a> {
    fn get_step_descriptions(kind: Kind) -> Vec<StepDescription> {
        macro_rules! describe {
            ($($rule:ty),+ $(,)*) => {{
                vec![$(StepDescription::from::<$rule>()),+]
            }};
        }
        match kind {
            Kind::Build => describe!(
                compile::Std,
                compile::Test,
                compile::Rustc,
                compile::CodegenBackend,
                compile::StartupObjects,
                tool::BuildManifest,
                tool::Rustbook,
                tool::ErrorIndex,
                tool::UnstableBookGen,
                tool::Tidy,
                tool::Linkchecker,
                tool::CargoTest,
                tool::Compiletest,
                tool::RemoteTestServer,
                tool::RemoteTestClient,
                tool::RustInstaller,
                tool::Cargo,
                tool::Rls,
                tool::Rustdoc,
                tool::Clippy,
                native::Llvm,
                tool::Rustfmt,
                tool::Miri,
                native::Lld
            ),
            Kind::Check => describe!(
                check::Std,
                check::Test,
                check::Rustc,
                check::CodegenBackend,
                check::Rustdoc
            ),
            Kind::Test => describe!(
                test::Tidy,
                test::Ui,
                test::RunPass,
                test::CompileFail,
                test::ParseFail,
                test::RunFail,
                test::RunPassValgrind,
                test::MirOpt,
                test::Codegen,
                test::CodegenUnits,
                test::Incremental,
                test::Debuginfo,
                test::UiFullDeps,
                test::RunPassFullDeps,
                test::RunFailFullDeps,
                test::CompileFailFullDeps,
                test::IncrementalFullDeps,
                test::Rustdoc,
                test::Pretty,
                test::RunPassPretty,
                test::RunFailPretty,
                test::RunPassValgrindPretty,
                test::RunPassFullDepsPretty,
                test::RunFailFullDepsPretty,
                test::Crate,
                test::CrateLibrustc,
                test::CrateRustdoc,
                test::Linkcheck,
                test::Cargotest,
                test::Cargo,
                test::Rls,
                test::ErrorIndex,
                test::Distcheck,
                test::RunMakeFullDeps,
                test::Nomicon,
                test::Reference,
                test::RustdocBook,
                test::RustByExample,
                test::TheBook,
                test::UnstableBook,
                test::RustcBook,
                test::Rustfmt,
                test::Miri,
                test::Clippy,
                test::RustdocJS,
                test::RustdocTheme,
                // Run bootstrap close to the end as it's unlikely to fail
                test::Bootstrap,
                // Run run-make last, since these won't pass without make on Windows
                test::RunMake,
                test::RustdocUi
            ),
            Kind::Bench => describe!(test::Crate, test::CrateLibrustc),
            Kind::Doc => describe!(
                doc::UnstableBook,
                doc::UnstableBookGen,
                doc::TheBook,
                doc::Standalone,
                doc::Std,
                doc::Test,
                doc::WhitelistedRustc,
                doc::Rustc,
                doc::Rustdoc,
                doc::ErrorIndex,
                doc::Nomicon,
                doc::Reference,
                doc::RustdocBook,
                doc::RustByExample,
                doc::RustcBook,
                doc::CargoBook
            ),
            Kind::Dist => describe!(
                dist::Docs,
                dist::RustcDocs,
                dist::Mingw,
                dist::Rustc,
                dist::DebuggerScripts,
                dist::Std,
                dist::Analysis,
                dist::Src,
                dist::PlainSourceTarball,
                dist::Cargo,
                dist::Rls,
                dist::Rustfmt,
                dist::Clippy,
                dist::LlvmTools,
                dist::Lldb,
                dist::Extended,
                dist::HashSign
            ),
            Kind::Install => describe!(
                install::Docs,
                install::Std,
                install::Cargo,
                install::Rls,
                install::Rustfmt,
                install::Clippy,
                install::Analysis,
                install::Src,
                install::Rustc
            ),
        }
    }

    pub fn get_help(build: &Build, subcommand: &str) -> Option<String> {
        let kind = match subcommand {
            "build" => Kind::Build,
            "doc" => Kind::Doc,
            "test" => Kind::Test,
            "bench" => Kind::Bench,
            "dist" => Kind::Dist,
            "install" => Kind::Install,
            _ => return None,
        };

        let builder = Builder {
            build,
            top_stage: build.config.stage.unwrap_or(2),
            kind,
            cache: Cache::new(),
            stack: RefCell::new(Vec::new()),
            time_spent_on_dependencies: Cell::new(Duration::new(0, 0)),
            paths: vec![],
            graph_nodes: RefCell::new(HashMap::new()),
            graph: RefCell::new(Graph::new()),
            parent: Cell::new(None),
        };

        let builder = &builder;
        let mut should_run = ShouldRun::new(builder);
        for desc in Builder::get_step_descriptions(builder.kind) {
            should_run = (desc.should_run)(should_run);
        }
        let mut help = String::from("Available paths:\n");
        for pathset in should_run.paths {
            if let PathSet::Set(set) = pathset {
                set.iter().for_each(|path| {
                    help.push_str(
                        format!("    ./x.py {} {}\n", subcommand, path.display()).as_str(),
                    )
                })
            }
        }
        Some(help)
    }

    pub fn new(build: &Build) -> Builder {
        let (kind, paths) = match build.config.cmd {
            Subcommand::Build { ref paths } => (Kind::Build, &paths[..]),
            Subcommand::Check { ref paths } => (Kind::Check, &paths[..]),
            Subcommand::Doc { ref paths } => (Kind::Doc, &paths[..]),
            Subcommand::Test { ref paths, .. } => (Kind::Test, &paths[..]),
            Subcommand::Bench { ref paths, .. } => (Kind::Bench, &paths[..]),
            Subcommand::Dist { ref paths } => (Kind::Dist, &paths[..]),
            Subcommand::Install { ref paths } => (Kind::Install, &paths[..]),
            Subcommand::Clean { .. } => panic!(),
        };

        let builder = Builder {
            build,
            top_stage: build.config.stage.unwrap_or(2),
            kind,
            cache: Cache::new(),
            stack: RefCell::new(Vec::new()),
            time_spent_on_dependencies: Cell::new(Duration::new(0, 0)),
            paths: paths.to_owned(),
            graph_nodes: RefCell::new(HashMap::new()),
            graph: RefCell::new(Graph::new()),
            parent: Cell::new(None),
        };

        if kind == Kind::Dist {
            assert!(
                !builder.config.test_miri,
                "Do not distribute with miri enabled.\n\
                The distributed libraries would include all MIR (increasing binary size).
                The distributed MIR would include validation statements."
            );
        }

        builder
    }

    pub fn execute_cli(&self) -> Graph<String, bool> {
        self.run_step_descriptions(&Builder::get_step_descriptions(self.kind), &self.paths);
        self.graph.borrow().clone()
    }

    pub fn default_doc(&self, paths: Option<&[PathBuf]>) {
        let paths = paths.unwrap_or(&[]);
        self.run_step_descriptions(&Builder::get_step_descriptions(Kind::Doc), paths);
    }

    fn run_step_descriptions(&self, v: &[StepDescription], paths: &[PathBuf]) {
        StepDescription::run(v, self, paths);
    }

    /// Obtain a compiler at a given stage and for a given host. Explicitly does
    /// not take `Compiler` since all `Compiler` instances are meant to be
    /// obtained through this function, since it ensures that they are valid
    /// (i.e., built and assembled).
    pub fn compiler(&self, stage: u32, host: Interned<String>) -> Compiler {
        self.ensure(compile::Assemble {
            target_compiler: Compiler { stage, host },
        })
    }

    pub fn sysroot(&self, compiler: Compiler) -> Interned<PathBuf> {
        self.ensure(compile::Sysroot { compiler })
    }

    /// Returns the libdir where the standard library and other artifacts are
    /// found for a compiler's sysroot.
    pub fn sysroot_libdir(
        &self,
        compiler: Compiler,
        target: Interned<String>,
    ) -> Interned<PathBuf> {
        #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
        struct Libdir {
            compiler: Compiler,
            target: Interned<String>,
        }
        impl Step for Libdir {
            type Output = Interned<PathBuf>;

            fn should_run(run: ShouldRun) -> ShouldRun {
                run.never()
            }

            fn run(self, builder: &Builder) -> Interned<PathBuf> {
                let compiler = self.compiler;
                let config = &builder.build.config;
                let lib = if compiler.stage >= 1 && config.libdir_relative().is_some() {
                    builder.build.config.libdir_relative().unwrap()
                } else {
                    Path::new("lib")
                };
                let sysroot = builder
                    .sysroot(self.compiler)
                    .join(lib)
                    .join("rustlib")
                    .join(self.target)
                    .join("lib");
                let _ = fs::remove_dir_all(&sysroot);
                t!(fs::create_dir_all(&sysroot));
                INTERNER.intern_path(sysroot)
            }
        }
        self.ensure(Libdir { compiler, target })
    }

    pub fn sysroot_codegen_backends(&self, compiler: Compiler) -> PathBuf {
        self.sysroot_libdir(compiler, compiler.host)
            .with_file_name(self.config.rust_codegen_backends_dir.clone())
    }

    /// Returns the compiler's libdir where it stores the dynamic libraries that
    /// it itself links against.
    ///
    /// For example this returns `<sysroot>/lib` on Unix and `<sysroot>/bin` on
    /// Windows.
    pub fn rustc_libdir(&self, compiler: Compiler) -> PathBuf {
        if compiler.is_snapshot(self) {
            self.rustc_snapshot_libdir()
        } else {
            self.sysroot(compiler).join(libdir(&compiler.host))
        }
    }

    /// Adds the compiler's directory of dynamic libraries to `cmd`'s dynamic
    /// library lookup path.
    pub fn add_rustc_lib_path(&self, compiler: Compiler, cmd: &mut Command) {
        // Windows doesn't need dylib path munging because the dlls for the
        // compiler live next to the compiler and the system will find them
        // automatically.
        if cfg!(windows) {
            return;
        }

        add_lib_path(vec![self.rustc_libdir(compiler)], cmd);
    }

    /// Get a path to the compiler specified.
    pub fn rustc(&self, compiler: Compiler) -> PathBuf {
        if compiler.is_snapshot(self) {
            self.initial_rustc.clone()
        } else {
            self.sysroot(compiler)
                .join("bin")
                .join(exe("rustc", &compiler.host))
        }
    }

    pub fn rustdoc(&self, host: Interned<String>) -> PathBuf {
        self.ensure(tool::Rustdoc { host })
    }

    pub fn rustdoc_cmd(&self, host: Interned<String>) -> Command {
        let mut cmd = Command::new(&self.out.join("bootstrap/debug/rustdoc"));
        let compiler = self.compiler(self.top_stage, host);
        cmd.env("RUSTC_STAGE", compiler.stage.to_string())
            .env("RUSTC_SYSROOT", self.sysroot(compiler))
            .env(
                "RUSTDOC_LIBDIR",
                self.sysroot_libdir(compiler, self.config.build),
            )
            .env("CFG_RELEASE_CHANNEL", &self.config.channel)
            .env("RUSTDOC_REAL", self.rustdoc(host))
            .env("RUSTDOC_CRATE_VERSION", self.rust_version())
            .env("RUSTC_BOOTSTRAP", "1");
        if let Some(linker) = self.linker(host) {
            cmd.env("RUSTC_TARGET_LINKER", linker);
        }
        cmd
    }

    /// Prepares an invocation of `cargo` to be run.
    ///
    /// This will create a `Command` that represents a pending execution of
    /// Cargo. This cargo will be configured to use `compiler` as the actual
    /// rustc compiler, its output will be scoped by `mode`'s output directory,
    /// it will pass the `--target` flag for the specified `target`, and will be
    /// executing the Cargo command `cmd`.
    pub fn cargo(
        &self,
        compiler: Compiler,
        mode: Mode,
        target: Interned<String>,
        cmd: &str,
    ) -> Command {
        let mut cargo = Command::new(&self.initial_cargo);
        let out_dir = self.stage_out(compiler, mode);
        cargo
            .env("CARGO_TARGET_DIR", out_dir)
            .arg(cmd);

        if cmd != "install" {
            cargo.arg("--target")
                 .arg(target);
        } else {
            assert_eq!(target, compiler.host);
        }

        // Set a flag for `check` so that certain build scripts can do less work
        // (e.g. not building/requiring LLVM).
        if cmd == "check" {
            cargo.env("RUST_CHECK", "1");
        }

        cargo.arg("-j").arg(self.jobs().to_string());
        // Remove make-related flags to ensure Cargo can correctly set things up
        cargo.env_remove("MAKEFLAGS");
        cargo.env_remove("MFLAGS");

        // FIXME: Temporary fix for https://github.com/rust-lang/cargo/issues/3005
        // Force cargo to output binaries with disambiguating hashes in the name
        let metadata = if compiler.stage == 0 {
            // Treat stage0 like special channel, whether it's a normal prior-
            // release rustc or a local rebuild with the same version, so we
            // never mix these libraries by accident.
            "bootstrap"
        } else {
            &self.config.channel
        };
        cargo.env("__CARGO_DEFAULT_LIB_METADATA", &metadata);

        let stage;
        if compiler.stage == 0 && self.local_rebuild {
            // Assume the local-rebuild rustc already has stage1 features.
            stage = 1;
        } else {
            stage = compiler.stage;
        }

        let mut extra_args = env::var(&format!("RUSTFLAGS_STAGE_{}", stage)).unwrap_or_default();
        if stage != 0 {
            let s = env::var("RUSTFLAGS_STAGE_NOT_0").unwrap_or_default();
            if !extra_args.is_empty() {
                extra_args.push_str(" ");
            }
            extra_args.push_str(&s);
        }

        if !extra_args.is_empty() {
            cargo.env(
                "RUSTFLAGS",
                format!(
                    "{} {}",
                    env::var("RUSTFLAGS").unwrap_or_default(),
                    extra_args
                ),
            );
        }

        let want_rustdoc = self.doc_tests != DocTests::No;

        // We synthetically interpret a stage0 compiler used to build tools as a
        // "raw" compiler in that it's the exact snapshot we download. Normally
        // the stage0 build means it uses libraries build by the stage0
        // compiler, but for tools we just use the precompiled libraries that
        // we've downloaded
        let use_snapshot = mode == Mode::ToolBootstrap;
        assert!(!use_snapshot || stage == 0 || self.local_rebuild);

        let maybe_sysroot = self.sysroot(compiler);
        let sysroot = if use_snapshot {
            self.rustc_snapshot_sysroot()
        } else {
            &maybe_sysroot
        };
        let libdir = sysroot.join(libdir(&compiler.host));

        // Customize the compiler we're running. Specify the compiler to cargo
        // as our shim and then pass it some various options used to configure
        // how the actual compiler itself is called.
        //
        // These variables are primarily all read by
        // src/bootstrap/bin/{rustc.rs,rustdoc.rs}
        cargo
            .env("RUSTBUILD_NATIVE_DIR", self.native_dir(target))
            .env("RUSTC", self.out.join("bootstrap/debug/rustc"))
            .env("RUSTC_REAL", self.rustc(compiler))
            .env("RUSTC_STAGE", stage.to_string())
            .env(
                "RUSTC_DEBUG_ASSERTIONS",
                self.config.rust_debug_assertions.to_string(),
            )
            .env("RUSTC_SYSROOT", &sysroot)
            .env("RUSTC_LIBDIR", &libdir)
            .env("RUSTC_RPATH", self.config.rust_rpath.to_string())
            .env("RUSTDOC", self.out.join("bootstrap/debug/rustdoc"))
            .env(
                "RUSTDOC_REAL",
                if cmd == "doc" || (cmd == "test" && want_rustdoc) {
                    self.rustdoc(compiler.host)
                } else {
                    PathBuf::from("/path/to/nowhere/rustdoc/not/required")
                },
            )
            .env("TEST_MIRI", self.config.test_miri.to_string())
            .env("RUSTC_ERROR_METADATA_DST", self.extended_error_dir());

        if let Some(host_linker) = self.linker(compiler.host) {
            cargo.env("RUSTC_HOST_LINKER", host_linker);
        }
        if let Some(target_linker) = self.linker(target) {
            cargo.env("RUSTC_TARGET_LINKER", target_linker);
        }
        if let Some(ref error_format) = self.config.rustc_error_format {
            cargo.env("RUSTC_ERROR_FORMAT", error_format);
        }
        if cmd != "build" && cmd != "check" && cmd != "rustc" && want_rustdoc {
            cargo.env("RUSTDOC_LIBDIR", self.sysroot_libdir(compiler, self.config.build));
        }

        if mode.is_tool() {
            // Tools like cargo and rls don't get debuginfo by default right now, but this can be
            // enabled in the config.  Adding debuginfo makes them several times larger.
            if self.config.rust_debuginfo_tools {
                cargo.env("RUSTC_DEBUGINFO", self.config.rust_debuginfo.to_string());
                cargo.env(
                    "RUSTC_DEBUGINFO_LINES",
                    self.config.rust_debuginfo_lines.to_string(),
                );
            }
        } else {
            cargo.env("RUSTC_DEBUGINFO", self.config.rust_debuginfo.to_string());
            cargo.env(
                "RUSTC_DEBUGINFO_LINES",
                self.config.rust_debuginfo_lines.to_string(),
            );
            cargo.env("RUSTC_FORCE_UNSTABLE", "1");

            // Currently the compiler depends on crates from crates.io, and
            // then other crates can depend on the compiler (e.g. proc-macro
            // crates). Let's say, for example that rustc itself depends on the
            // bitflags crate. If an external crate then depends on the
            // bitflags crate as well, we need to make sure they don't
            // conflict, even if they pick the same version of bitflags. We'll
            // want to make sure that e.g. a plugin and rustc each get their
            // own copy of bitflags.

            // Cargo ensures that this works in general through the -C metadata
            // flag. This flag will frob the symbols in the binary to make sure
            // they're different, even though the source code is the exact
            // same. To solve this problem for the compiler we extend Cargo's
            // already-passed -C metadata flag with our own. Our rustc.rs
            // wrapper around the actual rustc will detect -C metadata being
            // passed and frob it with this extra string we're passing in.
            cargo.env("RUSTC_METADATA_SUFFIX", "rustc");
        }

        if let Some(x) = self.crt_static(target) {
            cargo.env("RUSTC_CRT_STATIC", x.to_string());
        }

        if let Some(x) = self.crt_static(compiler.host) {
            cargo.env("RUSTC_HOST_CRT_STATIC", x.to_string());
        }

        // Enable usage of unstable features
        cargo.env("RUSTC_BOOTSTRAP", "1");
        self.add_rust_test_threads(&mut cargo);

        // Almost all of the crates that we compile as part of the bootstrap may
        // have a build script, including the standard library. To compile a
        // build script, however, it itself needs a standard library! This
        // introduces a bit of a pickle when we're compiling the standard
        // library itself.
        //
        // To work around this we actually end up using the snapshot compiler
        // (stage0) for compiling build scripts of the standard library itself.
        // The stage0 compiler is guaranteed to have a libstd available for use.
        //
        // For other crates, however, we know that we've already got a standard
        // library up and running, so we can use the normal compiler to compile
        // build scripts in that situation.
        //
        // If LLVM support is disabled we need to use the snapshot compiler to compile
        // build scripts, as the new compiler doesn't support executables.
        if mode == Mode::Std || !self.config.llvm_enabled {
            cargo
                .env("RUSTC_SNAPSHOT", &self.initial_rustc)
                .env("RUSTC_SNAPSHOT_LIBDIR", self.rustc_snapshot_libdir());
        } else {
            cargo
                .env("RUSTC_SNAPSHOT", self.rustc(compiler))
                .env("RUSTC_SNAPSHOT_LIBDIR", self.rustc_libdir(compiler));
        }

        if self.config.incremental {
            cargo.env("CARGO_INCREMENTAL", "1");
        }

        if let Some(ref on_fail) = self.config.on_fail {
            cargo.env("RUSTC_ON_FAIL", on_fail);
        }

        if self.config.print_step_timings {
            cargo.env("RUSTC_PRINT_STEP_TIMINGS", "1");
        }

        if self.config.backtrace_on_ice {
            cargo.env("RUSTC_BACKTRACE_ON_ICE", "1");
        }

        if self.config.rust_verify_llvm_ir {
            cargo.env("RUSTC_VERIFY_LLVM_IR", "1");
        }

        cargo.env("RUSTC_VERBOSE", self.verbosity.to_string());

        // in std, we want to avoid denying warnings for stage 0 as that makes cfg's painful.
        if self.config.deny_warnings && !(mode == Mode::Std && stage == 0) {
            cargo.env("RUSTC_DENY_WARNINGS", "1");
        }

        // Throughout the build Cargo can execute a number of build scripts
        // compiling C/C++ code and we need to pass compilers, archivers, flags, etc
        // obtained previously to those build scripts.
        // Build scripts use either the `cc` crate or `configure/make` so we pass
        // the options through environment variables that are fetched and understood by both.
        //
        // FIXME: the guard against msvc shouldn't need to be here
        if target.contains("msvc") {
            if let Some(ref cl) = self.config.llvm_clang_cl {
                cargo.env("CC", cl).env("CXX", cl);
            }
        } else {
            let ccache = self.config.ccache.as_ref();
            let ccacheify = |s: &Path| {
                let ccache = match ccache {
                    Some(ref s) => s,
                    None => return s.display().to_string(),
                };
                // FIXME: the cc-rs crate only recognizes the literal strings
                // `ccache` and `sccache` when doing caching compilations, so we
                // mirror that here. It should probably be fixed upstream to
                // accept a new env var or otherwise work with custom ccache
                // vars.
                match &ccache[..] {
                    "ccache" | "sccache" => format!("{} {}", ccache, s.display()),
                    _ => s.display().to_string(),
                }
            };
            let cc = ccacheify(&self.cc(target));
            cargo.env(format!("CC_{}", target), &cc).env("CC", &cc);

            let cflags = self.cflags(target).join(" ");
            cargo
                .env(format!("CFLAGS_{}", target), cflags.clone())
                .env("CFLAGS", cflags.clone());

            if let Some(ar) = self.ar(target) {
                let ranlib = format!("{} s", ar.display());
                cargo
                    .env(format!("AR_{}", target), ar)
                    .env("AR", ar)
                    .env(format!("RANLIB_{}", target), ranlib.clone())
                    .env("RANLIB", ranlib);
            }

            if let Ok(cxx) = self.cxx(target) {
                let cxx = ccacheify(&cxx);
                cargo
                    .env(format!("CXX_{}", target), &cxx)
                    .env("CXX", &cxx)
                    .env(format!("CXXFLAGS_{}", target), cflags.clone())
                    .env("CXXFLAGS", cflags);
            }
        }

        if (cmd == "build" || cmd == "rustc")
            && mode == Mode::Std
            && self.config.extended
            && compiler.is_final_stage(self)
        {
            cargo.env("RUSTC_SAVE_ANALYSIS", "api".to_string());
        }

        // For `cargo doc` invocations, make rustdoc print the Rust version into the docs
        cargo.env("RUSTDOC_CRATE_VERSION", self.rust_version());

        // Environment variables *required* throughout the build
        //
        // FIXME: should update code to not require this env var
        cargo.env("CFG_COMPILER_HOST_TRIPLE", target);

        // Set this for all builds to make sure doc builds also get it.
        cargo.env("CFG_RELEASE_CHANNEL", &self.config.channel);

        // This one's a bit tricky. As of the time of this writing the compiler
        // links to the `winapi` crate on crates.io. This crate provides raw
        // bindings to Windows system functions, sort of like libc does for
        // Unix. This crate also, however, provides "import libraries" for the
        // MinGW targets. There's an import library per dll in the windows
        // distribution which is what's linked to. These custom import libraries
        // are used because the winapi crate can reference Windows functions not
        // present in the MinGW import libraries.
        //
        // For example MinGW may ship libdbghelp.a, but it may not have
        // references to all the functions in the dbghelp dll. Instead the
        // custom import library for dbghelp in the winapi crates has all this
        // information.
        //
        // Unfortunately for us though the import libraries are linked by
        // default via `-ldylib=winapi_foo`. That is, they're linked with the
        // `dylib` type with a `winapi_` prefix (so the winapi ones don't
        // conflict with the system MinGW ones). This consequently means that
        // the binaries we ship of things like rustc_codegen_llvm (aka the rustc_codegen_llvm
        // DLL) when linked against *again*, for example with procedural macros
        // or plugins, will trigger the propagation logic of `-ldylib`, passing
        // `-lwinapi_foo` to the linker again. This isn't actually available in
        // our distribution, however, so the link fails.
        //
        // To solve this problem we tell winapi to not use its bundled import
        // libraries. This means that it will link to the system MinGW import
        // libraries by default, and the `-ldylib=foo` directives will still get
        // passed to the final linker, but they'll look like `-lfoo` which can
        // be resolved because MinGW has the import library. The downside is we
        // don't get newer functions from Windows, but we don't use any of them
        // anyway.
        if !mode.is_tool() {
            cargo.env("WINAPI_NO_BUNDLED_LIBRARIES", "1");
        }

        for _ in 1..self.verbosity {
            cargo.arg("-v");
        }

        // This must be kept before the thinlto check, as we set codegen units
        // to 1 forcibly there.
        if let Some(n) = self.config.rust_codegen_units {
            cargo.env("RUSTC_CODEGEN_UNITS", n.to_string());
        }

        if self.config.rust_optimize {
            // FIXME: cargo bench/install do not accept `--release`
            if cmd != "bench" && cmd != "install" {
                cargo.arg("--release");
            }
        }

        if self.config.locked_deps {
            cargo.arg("--locked");
        }
        if self.config.vendor || self.is_sudo {
            cargo.arg("--frozen");
        }

        self.ci_env.force_coloring_in_ci(&mut cargo);

        cargo
    }

    /// Ensure that a given step is built, returning its output. This will
    /// cache the step, so it is safe (and good!) to call this as often as
    /// needed to ensure that all dependencies are built.
    pub fn ensure<S: Step>(&'a self, step: S) -> S::Output {
        {
            let mut stack = self.stack.borrow_mut();
            for stack_step in stack.iter() {
                // should skip
                if stack_step
                    .downcast_ref::<S>()
                    .map_or(true, |stack_step| *stack_step != step)
                {
                    continue;
                }
                let mut out = String::new();
                out += &format!("\n\nCycle in build detected when adding {:?}\n", step);
                for el in stack.iter().rev() {
                    out += &format!("\t{:?}\n", el);
                }
                panic!(out);
            }
            if let Some(out) = self.cache.get(&step) {
                self.verbose(&format!("{}c {:?}", "  ".repeat(stack.len()), step));

                {
                    let mut graph = self.graph.borrow_mut();
                    let parent = self.parent.get();
                    let us = *self
                        .graph_nodes
                        .borrow_mut()
                        .entry(format!("{:?}", step))
                        .or_insert_with(|| graph.add_node(format!("{:?}", step)));
                    if let Some(parent) = parent {
                        graph.add_edge(parent, us, false);
                    }
                }

                return out;
            }
            self.verbose(&format!("{}> {:?}", "  ".repeat(stack.len()), step));
            stack.push(Box::new(step.clone()));
        }

        let prev_parent = self.parent.get();

        {
            let mut graph = self.graph.borrow_mut();
            let parent = self.parent.get();
            let us = *self
                .graph_nodes
                .borrow_mut()
                .entry(format!("{:?}", step))
                .or_insert_with(|| graph.add_node(format!("{:?}", step)));
            self.parent.set(Some(us));
            if let Some(parent) = parent {
                graph.add_edge(parent, us, true);
            }
        }

        let (out, dur) = {
            let start = Instant::now();
            let zero = Duration::new(0, 0);
            let parent = self.time_spent_on_dependencies.replace(zero);
            let out = step.clone().run(self);
            let dur = start.elapsed();
            let deps = self.time_spent_on_dependencies.replace(parent + dur);
            (out, dur - deps)
        };

        self.parent.set(prev_parent);

        if self.config.print_step_timings && dur > Duration::from_millis(100) {
            println!(
                "[TIMING] {:?} -- {}.{:03}",
                step,
                dur.as_secs(),
                dur.subsec_nanos() / 1_000_000
            );
        }

        {
            let mut stack = self.stack.borrow_mut();
            let cur_step = stack.pop().expect("step stack empty");
            assert_eq!(cur_step.downcast_ref(), Some(&step));
        }
        self.verbose(&format!(
            "{}< {:?}",
            "  ".repeat(self.stack.borrow().len()),
            step
        ));
        self.cache.put(step, out.clone());
        out
    }
}

#[cfg(test)]
mod __test {
    use super::*;
    use config::Config;
    use std::thread;

    fn configure(host: &[&str], target: &[&str]) -> Config {
        let mut config = Config::default_opts();
        // don't save toolstates
        config.save_toolstates = None;
        config.run_host_only = true;
        config.dry_run = true;
        // try to avoid spurious failures in dist where we create/delete each others file
        let dir = config.out.join("tmp-rustbuild-tests").join(
            &thread::current()
                .name()
                .unwrap_or("unknown")
                .replace(":", "-"),
        );
        t!(fs::create_dir_all(&dir));
        config.out = dir;
        config.build = INTERNER.intern_str("A");
        config.hosts = vec![config.build]
            .clone()
            .into_iter()
            .chain(host.iter().map(|s| INTERNER.intern_str(s)))
            .collect::<Vec<_>>();
        config.targets = config
            .hosts
            .clone()
            .into_iter()
            .chain(target.iter().map(|s| INTERNER.intern_str(s)))
            .collect::<Vec<_>>();
        config
    }

    fn first<A, B>(v: Vec<(A, B)>) -> Vec<A> {
        v.into_iter().map(|(a, _)| a).collect::<Vec<_>>()
    }

    #[test]
    fn dist_baseline() {
        let build = Build::new(configure(&[], &[]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[dist::Docs { stage: 2, host: a },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[dist::Mingw { host: a },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Rustc>()),
            &[dist::Rustc {
                compiler: Compiler { host: a, stage: 2 }
            },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[dist::Std {
                compiler: Compiler { host: a, stage: 2 },
                target: a,
            },]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[dist::Src]);
    }

    #[test]
    fn dist_with_targets() {
        let build = Build::new(configure(&[], &["B"]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[
                dist::Docs { stage: 2, host: a },
                dist::Docs { stage: 2, host: b },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[dist::Mingw { host: a }, dist::Mingw { host: b },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Rustc>()),
            &[dist::Rustc {
                compiler: Compiler { host: a, stage: 2 }
            },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[dist::Src]);
    }

    #[test]
    fn dist_with_hosts() {
        let build = Build::new(configure(&["B"], &[]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[
                dist::Docs { stage: 2, host: a },
                dist::Docs { stage: 2, host: b },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[dist::Mingw { host: a }, dist::Mingw { host: b },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Rustc>()),
            &[
                dist::Rustc {
                    compiler: Compiler { host: a, stage: 2 }
                },
                dist::Rustc {
                    compiler: Compiler { host: b, stage: 2 }
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[dist::Src]);
    }

    #[test]
    fn dist_with_targets_and_hosts() {
        let build = Build::new(configure(&["B"], &["C"]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");
        let c = INTERNER.intern_str("C");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[
                dist::Docs { stage: 2, host: a },
                dist::Docs { stage: 2, host: b },
                dist::Docs { stage: 2, host: c },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[
                dist::Mingw { host: a },
                dist::Mingw { host: b },
                dist::Mingw { host: c },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Rustc>()),
            &[
                dist::Rustc {
                    compiler: Compiler { host: a, stage: 2 }
                },
                dist::Rustc {
                    compiler: Compiler { host: b, stage: 2 }
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: c,
                },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[dist::Src]);
    }

    #[test]
    fn dist_with_target_flag() {
        let mut config = configure(&["B"], &["C"]);
        config.run_host_only = false; // as-if --target=C was passed
        let build = Build::new(config);
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");
        let c = INTERNER.intern_str("C");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[
                dist::Docs { stage: 2, host: a },
                dist::Docs { stage: 2, host: b },
                dist::Docs { stage: 2, host: c },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[
                dist::Mingw { host: a },
                dist::Mingw { host: b },
                dist::Mingw { host: c },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Rustc>()), &[]);
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: c,
                },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[]);
    }

    #[test]
    fn dist_with_same_targets_and_hosts() {
        let build = Build::new(configure(&["B"], &["B"]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Dist), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");

        assert_eq!(
            first(builder.cache.all::<dist::Docs>()),
            &[
                dist::Docs { stage: 2, host: a },
                dist::Docs { stage: 2, host: b },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Mingw>()),
            &[dist::Mingw { host: a }, dist::Mingw { host: b },]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Rustc>()),
            &[
                dist::Rustc {
                    compiler: Compiler { host: a, stage: 2 }
                },
                dist::Rustc {
                    compiler: Compiler { host: b, stage: 2 }
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<dist::Std>()),
            &[
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                dist::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
            ]
        );
        assert_eq!(first(builder.cache.all::<dist::Src>()), &[dist::Src]);
        assert_eq!(
            first(builder.cache.all::<compile::Std>()),
            &[
                compile::Std {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Std {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                compile::Std {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
                compile::Std {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<compile::Test>()),
            &[
                compile::Test {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<compile::Assemble>()),
            &[
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 0 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 1 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 2 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: b, stage: 2 },
                },
            ]
        );
    }

    #[test]
    fn build_default() {
        let build = Build::new(configure(&["B"], &["C"]));
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Build), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");
        let c = INTERNER.intern_str("C");

        assert!(!builder.cache.all::<compile::Std>().is_empty());
        assert!(!builder.cache.all::<compile::Assemble>().is_empty());
        assert_eq!(
            first(builder.cache.all::<compile::Rustc>()),
            &[
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: b, stage: 2 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 0 },
                    target: b,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
                compile::Rustc {
                    compiler: Compiler { host: b, stage: 2 },
                    target: b,
                },
            ]
        );

        assert_eq!(
            first(builder.cache.all::<compile::Test>()),
            &[
                compile::Test {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 0 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: c,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: c,
                },
            ]
        );
    }

    #[test]
    fn build_with_target_flag() {
        let mut config = configure(&["B"], &["C"]);
        config.run_host_only = false;
        let build = Build::new(config);
        let mut builder = Builder::new(&build);
        builder.run_step_descriptions(&Builder::get_step_descriptions(Kind::Build), &[]);

        let a = INTERNER.intern_str("A");
        let b = INTERNER.intern_str("B");
        let c = INTERNER.intern_str("C");

        assert!(!builder.cache.all::<compile::Std>().is_empty());
        assert_eq!(
            first(builder.cache.all::<compile::Assemble>()),
            &[
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 0 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 1 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: b, stage: 1 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: a, stage: 2 },
                },
                compile::Assemble {
                    target_compiler: Compiler { host: b, stage: 2 },
                },
            ]
        );
        assert_eq!(
            first(builder.cache.all::<compile::Rustc>()),
            &[
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 0 },
                    target: b,
                },
                compile::Rustc {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
            ]
        );

        assert_eq!(
            first(builder.cache.all::<compile::Test>()),
            &[
                compile::Test {
                    compiler: Compiler { host: a, stage: 0 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: a,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 0 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 1 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: b,
                },
                compile::Test {
                    compiler: Compiler { host: a, stage: 2 },
                    target: c,
                },
                compile::Test {
                    compiler: Compiler { host: b, stage: 2 },
                    target: c,
                },
            ]
        );
    }

    #[test]
    fn test_with_no_doc_stage0() {
        let mut config = configure(&[], &[]);
        config.stage = Some(0);
        config.cmd = Subcommand::Test {
            paths: vec!["src/libstd".into()],
            test_args: vec![],
            rustc_args: vec![],
            fail_fast: true,
            doc_tests: DocTests::No,
            bless: false,
            compare_mode: None,
        };

        let build = Build::new(config);
        let mut builder = Builder::new(&build);

        let host = INTERNER.intern_str("A");

        builder.run_step_descriptions(
            &[StepDescription::from::<test::Crate>()],
            &["src/libstd".into()],
        );

        // Ensure we don't build any compiler artifacts.
        assert!(builder.cache.all::<compile::Rustc>().is_empty());
        assert_eq!(
            first(builder.cache.all::<test::Crate>()),
            &[test::Crate {
                compiler: Compiler { host, stage: 0 },
                target: host,
                mode: Mode::Std,
                test_kind: test::TestKind::Test,
                krate: INTERNER.intern_str("std"),
            },]
        );
    }
}
// Copyright 2017 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::any::{Any, TypeId};
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::AsRef;
use std::ffi::OsStr;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::cmp::{PartialOrd, Ord, Ordering};

use builder::Step;

pub struct Interned<T>(usize, PhantomData<*const T>);

impl Default for Interned<String> {
    fn default() -> Self {
        INTERNER.intern_string(String::default())
    }
}

impl Default for Interned<PathBuf> {
    fn default() -> Self {
        INTERNER.intern_path(PathBuf::default())
    }
}

impl<T> Copy for Interned<T> {}
impl<T> Clone for Interned<T> {
    fn clone(&self) -> Interned<T> {
        *self
    }
}

impl<T> PartialEq for Interned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}
impl<T> Eq for Interned<T> {}

impl PartialEq<str> for Interned<String> {
    fn eq(&self, other: &str) -> bool {
       *self == other
    }
}
impl<'a> PartialEq<&'a str> for Interned<String> {
    fn eq(&self, other: &&str) -> bool {
        **self == **other
    }
}
impl<'a, T> PartialEq<&'a Interned<T>> for Interned<T> {
    fn eq(&self, other: &&Self) -> bool {
        self.0 == other.0
    }
}
impl<'a, T> PartialEq<Interned<T>> for &'a Interned<T> {
    fn eq(&self, other: &Interned<T>) -> bool {
        self.0 == other.0
    }
}

unsafe impl<T> Send for Interned<T> {}
unsafe impl<T> Sync for Interned<T> {}

impl fmt::Display for Interned<String> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: &str = &*self;
        f.write_str(s)
    }
}

impl fmt::Debug for Interned<String> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: &str = &*self;
        f.write_fmt(format_args!("{:?}", s))
    }
}
impl fmt::Debug for Interned<PathBuf> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s: &Path = &*self;
        f.write_fmt(format_args!("{:?}", s))
    }
}

impl Hash for Interned<String> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let l = INTERNER.strs.lock().unwrap();
        l.get(*self).hash(state)
    }
}

impl Hash for Interned<PathBuf> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let l = INTERNER.paths.lock().unwrap();
        l.get(*self).hash(state)
    }
}

impl Deref for Interned<String> {
    type Target = str;
    fn deref(&self) -> &'static str {
        let l = INTERNER.strs.lock().unwrap();
        unsafe { mem::transmute::<&str, &'static str>(l.get(*self)) }
    }
}

impl Deref for Interned<PathBuf> {
    type Target = Path;
    fn deref(&self) -> &'static Path {
        let l = INTERNER.paths.lock().unwrap();
        unsafe { mem::transmute::<&Path, &'static Path>(l.get(*self)) }
    }
}

impl AsRef<Path> for Interned<PathBuf> {
    fn as_ref(&self) -> &'static Path {
        let l = INTERNER.paths.lock().unwrap();
        unsafe { mem::transmute::<&Path, &'static Path>(l.get(*self)) }
    }
}

impl AsRef<Path> for Interned<String> {
    fn as_ref(&self) -> &'static Path {
        let l = INTERNER.strs.lock().unwrap();
        unsafe { mem::transmute::<&Path, &'static Path>(l.get(*self).as_ref()) }
    }
}

impl AsRef<OsStr> for Interned<PathBuf> {
    fn as_ref(&self) -> &'static OsStr {
        let l = INTERNER.paths.lock().unwrap();
        unsafe { mem::transmute::<&OsStr, &'static OsStr>(l.get(*self).as_ref()) }
    }
}

impl AsRef<OsStr> for Interned<String> {
    fn as_ref(&self) -> &'static OsStr {
        let l = INTERNER.strs.lock().unwrap();
        unsafe { mem::transmute::<&OsStr, &'static OsStr>(l.get(*self).as_ref()) }
    }
}

impl PartialOrd<Interned<String>> for Interned<String> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let l = INTERNER.strs.lock().unwrap();
        l.get(*self).partial_cmp(l.get(*other))
    }
}

impl Ord for Interned<String> {
    fn cmp(&self, other: &Self) -> Ordering {
        let l = INTERNER.strs.lock().unwrap();
        l.get(*self).cmp(l.get(*other))
    }
}

struct TyIntern<T> {
    items: Vec<T>,
    set: HashMap<T, Interned<T>>,
}

impl<T: Hash + Clone + Eq> TyIntern<T> {
    fn new() -> TyIntern<T> {
        TyIntern {
            items: Vec::new(),
            set: HashMap::new(),
        }
    }

    fn intern_borrow<B>(&mut self, item: &B) -> Interned<T>
    where
        B: Eq + Hash + ToOwned<Owned=T> + ?Sized,
        T: Borrow<B>,
    {
        if let Some(i) = self.set.get(&item) {
            return *i;
        }
        let item = item.to_owned();
        let interned =  Interned(self.items.len(), PhantomData::<*const T>);
        self.set.insert(item.clone(), interned);
        self.items.push(item);
        interned
    }

    fn intern(&mut self, item: T) -> Interned<T> {
        if let Some(i) = self.set.get(&item) {
            return *i;
        }
        let interned =  Interned(self.items.len(), PhantomData::<*const T>);
        self.set.insert(item.clone(), interned);
        self.items.push(item);
        interned
    }

    fn get(&self, i: Interned<T>) -> &T {
        &self.items[i.0]
    }
}

pub struct Interner {
    strs: Mutex<TyIntern<String>>,
    paths: Mutex<TyIntern<PathBuf>>,
}

impl Interner {
    fn new() -> Interner {
        Interner {
            strs: Mutex::new(TyIntern::new()),
            paths: Mutex::new(TyIntern::new()),
        }
    }

    pub fn intern_str(&self, s: &str) -> Interned<String> {
        self.strs.lock().unwrap().intern_borrow(s)
    }
    pub fn intern_string(&self, s: String) -> Interned<String> {
        self.strs.lock().unwrap().intern(s)
    }

    pub fn intern_path(&self, s: PathBuf) -> Interned<PathBuf> {
        self.paths.lock().unwrap().intern(s)
    }
}

lazy_static! {
    pub static ref INTERNER: Interner = Interner::new();
}

/// This is essentially a HashMap which allows storing any type in its input and
/// any type in its output. It is a write-once cache; values are never evicted,
/// which means that references to the value can safely be returned from the
/// get() method.
#[derive(Debug)]
pub struct Cache(
    RefCell<HashMap<
        TypeId,
        Box<dyn Any>, // actually a HashMap<Step, Interned<Step::Output>>
    >>
);

impl Cache {
    pub fn new() -> Cache {
        Cache(RefCell::new(HashMap::new()))
    }

    pub fn put<S: Step>(&self, step: S, value: S::Output) {
        let mut cache = self.0.borrow_mut();
        let type_id = TypeId::of::<S>();
        let stepcache = cache.entry(type_id)
                        .or_insert_with(|| Box::new(HashMap::<S, S::Output>::new()))
                        .downcast_mut::<HashMap<S, S::Output>>()
                        .expect("invalid type mapped");
        assert!(!stepcache.contains_key(&step), "processing {:?} a second time", step);
        stepcache.insert(step, value);
    }

    pub fn get<S: Step>(&self, step: &S) -> Option<S::Output> {
        let mut cache = self.0.borrow_mut();
        let type_id = TypeId::of::<S>();
        let stepcache = cache.entry(type_id)
                        .or_insert_with(|| Box::new(HashMap::<S, S::Output>::new()))
                        .downcast_mut::<HashMap<S, S::Output>>()
                        .expect("invalid type mapped");
        stepcache.get(step).cloned()
    }

    #[cfg(test)]
    pub fn all<S: Ord + Copy + Step>(&mut self) -> Vec<(S, S::Output)> {
        let cache = self.0.get_mut();
        let type_id = TypeId::of::<S>();
        let mut v = cache.remove(&type_id)
            .map(|b| b.downcast::<HashMap<S, S::Output>>().expect("correct type"))
            .map(|m| m.into_iter().collect::<Vec<_>>())
            .unwrap_or_default();
        v.sort_by_key(|&(a, _)| a);
        v
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// C-compiler probing and detection.
//
// This module will fill out the `cc` and `cxx` maps of `Build` by looking for
// C and C++ compilers for each target configured. A compiler is found through
// a number of vectors (in order of precedence)
//
// 1. Configuration via `target.$target.cc` in `config.toml`.
// 2. Configuration via `target.$target.android-ndk` in `config.toml`, if
//    applicable
// 3. Special logic to probe on OpenBSD
// 4. The `CC_$target` environment variable.
// 5. The `CC` environment variable.
// 6. "cc"
//
// Some of this logic is implemented here, but much of it is farmed out to the
// `cc` crate itself, so we end up having the same fallbacks as there.
// Similar logic is then used to find a C++ compiler, just some s/cc/c++/ is
// used.
//
// It is intended that after this module has run no C/C++ compiler will
// ever be probed for. Instead the compilers found here will be used for
// everything.

use std::collections::HashSet;
use std::{env, iter};
use std::path::{Path, PathBuf};
use std::process::Command;

use build_helper::output;
use cc;

use Build;
use config::Target;
use cache::Interned;

// The `cc` crate doesn't provide a way to obtain a path to the detected archiver,
// so use some simplified logic here. First we respect the environment variable `AR`, then
// try to infer the archiver path from the C compiler path.
// In the future this logic should be replaced by calling into the `cc` crate.
fn cc2ar(cc: &Path, target: &str) -> Option<PathBuf> {
    if let Some(ar) = env::var_os("AR") {
        Some(PathBuf::from(ar))
    } else if target.contains("msvc") {
        None
    } else if target.contains("musl") {
        Some(PathBuf::from("ar"))
    } else if target.contains("openbsd") {
        Some(PathBuf::from("ar"))
    } else {
        let parent = cc.parent().unwrap();
        let file = cc.file_name().unwrap().to_str().unwrap();
        for suffix in &["gcc", "cc", "clang"] {
            if let Some(idx) = file.rfind(suffix) {
                let mut file = file[..idx].to_owned();
                file.push_str("ar");
                return Some(parent.join(&file));
            }
        }
        Some(parent.join(file))
    }
}

pub fn find(build: &mut Build) {
    // For all targets we're going to need a C compiler for building some shims
    // and such as well as for being a linker for Rust code.
    let targets = build.targets.iter().chain(&build.hosts).cloned().chain(iter::once(build.build))
                               .collect::<HashSet<_>>();
    for target in targets.into_iter() {
        let mut cfg = cc::Build::new();
        cfg.cargo_metadata(false).opt_level(2).warnings(false).debug(false)
           .target(&target).host(&build.build);
        match build.crt_static(target) {
            Some(a) => { cfg.static_crt(a); }
            None => {
                if target.contains("msvc") {
                    cfg.static_crt(true);
                }
                if target.contains("musl") {
                    cfg.static_flag(true);
                }
            }
        }

        let config = build.config.target_config.get(&target);
        if let Some(cc) = config.and_then(|c| c.cc.as_ref()) {
            cfg.compiler(cc);
        } else {
            set_compiler(&mut cfg, Language::C, target, config, build);
        }

        let compiler = cfg.get_compiler();
        let ar = if let ar @ Some(..) = config.and_then(|c| c.ar.clone()) {
            ar
        } else {
            cc2ar(compiler.path(), &target)
        };

        build.cc.insert(target, compiler);
        build.verbose(&format!("CC_{} = {:?}", &target, build.cc(target)));
        build.verbose(&format!("CFLAGS_{} = {:?}", &target, build.cflags(target)));
        if let Some(ar) = ar {
            build.verbose(&format!("AR_{} = {:?}", &target, ar));
            build.ar.insert(target, ar);
        }
    }

    // For all host triples we need to find a C++ compiler as well
    let hosts = build.hosts.iter().cloned().chain(iter::once(build.build)).collect::<HashSet<_>>();
    for host in hosts.into_iter() {
        let mut cfg = cc::Build::new();
        cfg.cargo_metadata(false).opt_level(2).warnings(false).debug(false).cpp(true)
           .target(&host).host(&build.build);
        let config = build.config.target_config.get(&host);
        if let Some(cxx) = config.and_then(|c| c.cxx.as_ref()) {
            cfg.compiler(cxx);
        } else {
            set_compiler(&mut cfg, Language::CPlusPlus, host, config, build);
        }
        let compiler = cfg.get_compiler();
        build.verbose(&format!("CXX_{} = {:?}", host, compiler.path()));
        build.cxx.insert(host, compiler);
    }
}

fn set_compiler(cfg: &mut cc::Build,
                compiler: Language,
                target: Interned<String>,
                config: Option<&Target>,
                build: &Build) {
    match &*target {
        // When compiling for android we may have the NDK configured in the
        // config.toml in which case we look there. Otherwise the default
        // compiler already takes into account the triple in question.
        t if t.contains("android") => {
            if let Some(ndk) = config.and_then(|c| c.ndk.as_ref()) {
                let target = target.replace("armv7", "arm");
                let compiler = format!("{}-{}", target, compiler.clang());
                cfg.compiler(ndk.join("bin").join(compiler));
            }
        }

        // The default gcc version from OpenBSD may be too old, try using egcc,
        // which is a gcc version from ports, if this is the case.
        t if t.contains("openbsd") => {
            let c = cfg.get_compiler();
            let gnu_compiler = compiler.gcc();
            if !c.path().ends_with(gnu_compiler) {
                return
            }

            let output = output(c.to_command().arg("--version"));
            let i = match output.find(" 4.") {
                Some(i) => i,
                None => return,
            };
            match output[i + 3..].chars().next().unwrap() {
                '0' ... '6' => {}
                _ => return,
            }
            let alternative = format!("e{}", gnu_compiler);
            if Command::new(&alternative).output().is_ok() {
                cfg.compiler(alternative);
            }
        }

        "mips-unknown-linux-musl" => {
            if cfg.get_compiler().path().to_str() == Some("gcc") {
                cfg.compiler("mips-linux-musl-gcc");
            }
        }
        "mipsel-unknown-linux-musl" => {
            if cfg.get_compiler().path().to_str() == Some("gcc") {
                cfg.compiler("mipsel-linux-musl-gcc");
            }
        }

        t if t.contains("musl") => {
            if let Some(root) = build.musl_root(target) {
                let guess = root.join("bin/musl-gcc");
                if guess.exists() {
                    cfg.compiler(guess);
                }
            }
        }

        _ => {}
    }
}

/// The target programming language for a native compiler.
enum Language {
    /// The compiler is targeting C.
    C,
    /// The compiler is targeting C++.
    CPlusPlus,
}

impl Language {
    /// Obtains the name of a compiler in the GCC collection.
    fn gcc(self) -> &'static str {
        match self {
            Language::C => "gcc",
            Language::CPlusPlus => "g++",
        }
    }

    /// Obtains the name of a compiler in the clang suite.
    fn clang(self) -> &'static str {
        match self {
            Language::C => "clang",
            Language::CPlusPlus => "clang++",
        }
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Build configuration for Rust's release channels.
//
// Implements the stable/beta/nightly channel distinctions by setting various
// flags like the `unstable_features`, calculating variables like `release` and
// `package_vers`, and otherwise indicating to the compiler what it should
// print out as part of its version information.

use std::path::Path;
use std::process::Command;

use build_helper::output;

use Build;
use config::Config;

// The version number
pub const CFG_RELEASE_NUM: &str = "1.30.0";

pub struct GitInfo {
    inner: Option<Info>,
}

struct Info {
    commit_date: String,
    sha: String,
    short_sha: String,
}

impl GitInfo {
    pub fn new(config: &Config, dir: &Path) -> GitInfo {
        // See if this even begins to look like a git dir
        if config.ignore_git || !dir.join(".git").exists() {
            return GitInfo { inner: None }
        }

        // Make sure git commands work
        let out = Command::new("git")
                          .arg("rev-parse")
                          .current_dir(dir)
                          .output()
                          .expect("failed to spawn git");
        if !out.status.success() {
            return GitInfo { inner: None }
        }

        // Ok, let's scrape some info
        let ver_date = output(Command::new("git").current_dir(dir)
                                      .arg("log").arg("-1")
                                      .arg("--date=short")
                                      .arg("--pretty=format:%cd"));
        let ver_hash = output(Command::new("git").current_dir(dir)
                                      .arg("rev-parse").arg("HEAD"));
        let short_ver_hash = output(Command::new("git")
                                            .current_dir(dir)
                                            .arg("rev-parse")
                                            .arg("--short=9")
                                            .arg("HEAD"));
        GitInfo {
            inner: Some(Info {
                commit_date: ver_date.trim().to_string(),
                sha: ver_hash.trim().to_string(),
                short_sha: short_ver_hash.trim().to_string(),
            }),
        }
    }

    pub fn sha(&self) -> Option<&str> {
        self.inner.as_ref().map(|s| &s.sha[..])
    }

    pub fn sha_short(&self) -> Option<&str> {
        self.inner.as_ref().map(|s| &s.short_sha[..])
    }

    pub fn commit_date(&self) -> Option<&str> {
        self.inner.as_ref().map(|s| &s.commit_date[..])
    }

    pub fn version(&self, build: &Build, num: &str) -> String {
        let mut version = build.release(num);
        if let Some(ref inner) = self.inner {
            version.push_str(" (");
            version.push_str(&inner.short_sha);
            version.push_str(" ");
            version.push_str(&inner.commit_date);
            version.push_str(")");
        }
        version
    }

    pub fn is_git(&self) -> bool {
        self.inner.is_some()
    }
}
// Copyright 2018 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of compiling the compiler and standard library, in "check" mode.

use compile::{run_cargo, std_cargo, test_cargo, rustc_cargo, rustc_cargo_env, add_to_sysroot};
use builder::{RunConfig, Builder, ShouldRun, Step};
use tool::{self, prepare_tool_cargo, SourceType};
use {Compiler, Mode};
use cache::{INTERNER, Interned};
use std::path::PathBuf;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Std {
    pub target: Interned<String>,
}

impl Step for Std {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("std")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Std {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) {
        let target = self.target;
        let compiler = builder.compiler(0, builder.config.build);

        let out_dir = builder.stage_out(compiler, Mode::Std);
        builder.clear_if_dirty(&out_dir, &builder.rustc(compiler));

        let mut cargo = builder.cargo(compiler, Mode::Std, target, "check");
        std_cargo(builder, &compiler, target, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-std", compiler.stage));
        println!("Checking std artifacts ({} -> {})", &compiler.host, target);
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &libstd_stamp(builder, compiler, target),
                  true);

        let libdir = builder.sysroot_libdir(compiler, target);
        add_to_sysroot(&builder, &libdir, &libstd_stamp(builder, compiler, target));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Rustc {
    pub target: Interned<String>,
}

impl Step for Rustc {
    type Output = ();
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("rustc-main")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustc {
            target: run.target,
        });
    }

    /// Build the compiler.
    ///
    /// This will build the compiler for a particular stage of the build using
    /// the `compiler` targeting the `target` architecture. The artifacts
    /// created will also be linked into the sysroot directory.
    fn run(self, builder: &Builder) {
        let compiler = builder.compiler(0, builder.config.build);
        let target = self.target;

        let stage_out = builder.stage_out(compiler, Mode::Rustc);
        builder.clear_if_dirty(&stage_out, &libstd_stamp(builder, compiler, target));
        builder.clear_if_dirty(&stage_out, &libtest_stamp(builder, compiler, target));

        let mut cargo = builder.cargo(compiler, Mode::Rustc, target, "check");
        rustc_cargo(builder, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-rustc", compiler.stage));
        println!("Checking compiler artifacts ({} -> {})", &compiler.host, target);
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &librustc_stamp(builder, compiler, target),
                  true);

        let libdir = builder.sysroot_libdir(compiler, target);
        add_to_sysroot(&builder, &libdir, &librustc_stamp(builder, compiler, target));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CodegenBackend {
    pub target: Interned<String>,
    pub backend: Interned<String>,
}

impl Step for CodegenBackend {
    type Output = ();
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("rustc_codegen_llvm")
    }

    fn make_run(run: RunConfig) {
        let backend = run.builder.config.rust_codegen_backends.get(0);
        let backend = backend.cloned().unwrap_or_else(|| {
            INTERNER.intern_str("llvm")
        });
        run.builder.ensure(CodegenBackend {
            target: run.target,
            backend,
        });
    }

    fn run(self, builder: &Builder) {
        let compiler = builder.compiler(0, builder.config.build);
        let target = self.target;
        let backend = self.backend;

        let out_dir = builder.cargo_out(compiler, Mode::Codegen, target);
        builder.clear_if_dirty(&out_dir, &librustc_stamp(builder, compiler, target));

        let mut cargo = builder.cargo(compiler, Mode::Codegen, target, "check");
        cargo.arg("--manifest-path").arg(builder.src.join("src/librustc_codegen_llvm/Cargo.toml"));
        rustc_cargo_env(builder, &mut cargo);

        // We won't build LLVM if it's not available, as it shouldn't affect `check`.

        let _folder = builder.fold_output(|| format!("stage{}-rustc_codegen_llvm", compiler.stage));
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &codegen_backend_stamp(builder, compiler, target, backend),
                  true);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Test {
    pub target: Interned<String>,
}

impl Step for Test {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("test")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Test {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) {
        let compiler = builder.compiler(0, builder.config.build);
        let target = self.target;

        let out_dir = builder.stage_out(compiler, Mode::Test);
        builder.clear_if_dirty(&out_dir, &libstd_stamp(builder, compiler, target));

        let mut cargo = builder.cargo(compiler, Mode::Test, target, "check");
        test_cargo(builder, &compiler, target, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-test", compiler.stage));
        println!("Checking test artifacts ({} -> {})", &compiler.host, target);
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &libtest_stamp(builder, compiler, target),
                  true);

        let libdir = builder.sysroot_libdir(compiler, target);
        add_to_sysroot(builder, &libdir, &libtest_stamp(builder, compiler, target));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Rustdoc {
    pub target: Interned<String>,
}

impl Step for Rustdoc {
    type Output = ();
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/tools/rustdoc")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustdoc {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) {
        let compiler = builder.compiler(0, builder.config.build);
        let target = self.target;

        let stage_out = builder.stage_out(compiler, Mode::ToolRustc);
        builder.clear_if_dirty(&stage_out, &libstd_stamp(builder, compiler, target));
        builder.clear_if_dirty(&stage_out, &libtest_stamp(builder, compiler, target));
        builder.clear_if_dirty(&stage_out, &librustc_stamp(builder, compiler, target));

        let mut cargo = prepare_tool_cargo(builder,
                                           compiler,
                                           Mode::ToolRustc,
                                           target,
                                           "check",
                                           "src/tools/rustdoc",
                                           SourceType::InTree);

        let _folder = builder.fold_output(|| format!("stage{}-rustdoc", compiler.stage));
        println!("Checking rustdoc artifacts ({} -> {})", &compiler.host, target);
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &rustdoc_stamp(builder, compiler, target),
                  true);

        let libdir = builder.sysroot_libdir(compiler, target);
        add_to_sysroot(&builder, &libdir, &rustdoc_stamp(builder, compiler, target));

        builder.ensure(tool::CleanTools {
            compiler,
            target,
            cause: Mode::Rustc,
        });
    }
}

/// Cargo's output path for the standard library in a given stage, compiled
/// by a particular compiler for the specified target.
pub fn libstd_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Std, target).join(".libstd-check.stamp")
}

/// Cargo's output path for libtest in a given stage, compiled by a particular
/// compiler for the specified target.
pub fn libtest_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Test, target).join(".libtest-check.stamp")
}

/// Cargo's output path for librustc in a given stage, compiled by a particular
/// compiler for the specified target.
pub fn librustc_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Rustc, target).join(".librustc-check.stamp")
}

/// Cargo's output path for librustc_codegen_llvm in a given stage, compiled by a particular
/// compiler for the specified target and backend.
fn codegen_backend_stamp(builder: &Builder,
                         compiler: Compiler,
                         target: Interned<String>,
                         backend: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Codegen, target)
         .join(format!(".librustc_codegen_llvm-{}-check.stamp", backend))
}

/// Cargo's output path for rustdoc in a given stage, compiled by a particular
/// compiler for the specified target.
pub fn rustdoc_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::ToolRustc, target)
        .join(".rustdoc-check.stamp")
}
// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of `make clean` in rustbuild.
//
// Responsible for cleaning out a build directory of all old and stale
// artifacts to prepare for a fresh build. Currently doesn't remove the
// `build/cache` directory (download cache) or the `build/$target/llvm`
// directory unless the --all flag is present.

use std::fs;
use std::io::{self, ErrorKind};
use std::path::Path;

use Build;

pub fn clean(build: &Build, all: bool) {
    rm_rf("tmp".as_ref());

    if all {
        rm_rf(&build.out);
    } else {
        rm_rf(&build.out.join("tmp"));
        rm_rf(&build.out.join("dist"));

        for host in &build.hosts {
            let entries = match build.out.join(host).read_dir() {
                Ok(iter) => iter,
                Err(_) => continue,
            };

            for entry in entries {
                let entry = t!(entry);
                if entry.file_name().to_str() == Some("llvm") {
                    continue
                }
                let path = t!(entry.path().canonicalize());
                rm_rf(&path);
            }
        }
    }
}

fn rm_rf(path: &Path) {
    match path.symlink_metadata() {
        Err(e) => {
            if e.kind() == ErrorKind::NotFound {
                return;
            }
            panic!("failed to get metadata for file {}: {}", path.display(), e);
        },
        Ok(metadata) => {
            if metadata.file_type().is_file() || metadata.file_type().is_symlink() {
                do_op(path, "remove file", |p| fs::remove_file(p));
                return;
            }

            for file in t!(fs::read_dir(path)) {
                rm_rf(&t!(file).path());
            }
            do_op(path, "remove dir", |p| fs::remove_dir(p));
        },
    };
}

fn do_op<F>(path: &Path, desc: &str, mut f: F)
    where F: FnMut(&Path) -> io::Result<()>
{
    match f(path) {
        Ok(()) => {}
        // On windows we can't remove a readonly file, and git will often clone files as readonly.
        // As a result, we have some special logic to remove readonly files on windows.
        // This is also the reason that we can't use things like fs::remove_dir_all().
        Err(ref e) if cfg!(windows) &&
                      e.kind() == ErrorKind::PermissionDenied => {
            let mut p = t!(path.symlink_metadata()).permissions();
            p.set_readonly(false);
            t!(fs::set_permissions(path, p));
            f(path).unwrap_or_else(|e| {
                panic!("failed to {} {}: {}", desc, path.display(), e);
            })
        }
        Err(e) => {
            panic!("failed to {} {}: {}", desc, path.display(), e);
        }
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of compiling various phases of the compiler and standard
// library.
//
// This module contains some of the real meat in the rustbuild build system
// which is where Cargo is used to compiler the standard library, libtest, and
// compiler. This module is also responsible for assembling the sysroot as it
// goes along from the output of the previous stage.

use std::borrow::Cow;
use std::env;
use std::fs::{self, File};
use std::io::BufReader;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::str;

use build_helper::{output, mtime, up_to_date};
use filetime::FileTime;
use serde_json;

use util::{exe, libdir, is_dylib, CiEnv};
use {Compiler, Mode};
use native;
use tool;

use cache::{INTERNER, Interned};
use builder::{Step, RunConfig, ShouldRun, Builder};

#[derive(Debug, PartialOrd, Ord, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Std {
    pub target: Interned<String>,
    pub compiler: Compiler,
}

impl Step for Std {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("std")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Std {
            compiler: run.builder.compiler(run.builder.top_stage, run.host),
            target: run.target,
        });
    }

    /// Build the standard library.
    ///
    /// This will build the standard library for a particular stage of the build
    /// using the `compiler` targeting the `target` architecture. The artifacts
    /// created will also be linked into the sysroot directory.
    fn run(self, builder: &Builder) {
        let target = self.target;
        let compiler = self.compiler;

        if builder.config.keep_stage.contains(&compiler.stage) {
            builder.info("Warning: Using a potentially old libstd. This may not behave well.");
            builder.ensure(StdLink {
                compiler: compiler,
                target_compiler: compiler,
                target,
            });
            return;
        }

        builder.ensure(StartupObjects { compiler, target });

        if builder.force_use_stage1(compiler, target) {
            let from = builder.compiler(1, builder.config.build);
            builder.ensure(Std {
                compiler: from,
                target,
            });
            builder.info(&format!("Uplifting stage1 std ({} -> {})", from.host, target));

            // Even if we're not building std this stage, the new sysroot must
            // still contain the musl startup objects.
            if target.contains("musl") {
                let libdir = builder.sysroot_libdir(compiler, target);
                copy_musl_third_party_objects(builder, target, &libdir);
            }

            builder.ensure(StdLink {
                compiler: from,
                target_compiler: compiler,
                target,
            });
            return;
        }

        if target.contains("musl") {
            let libdir = builder.sysroot_libdir(compiler, target);
            copy_musl_third_party_objects(builder, target, &libdir);
        }

        let out_dir = builder.cargo_out(compiler, Mode::Std, target);
        builder.clear_if_dirty(&out_dir, &builder.rustc(compiler));
        let mut cargo = builder.cargo(compiler, Mode::Std, target, "build");
        std_cargo(builder, &compiler, target, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-std", compiler.stage));
        builder.info(&format!("Building stage{} std artifacts ({} -> {})", compiler.stage,
                &compiler.host, target));
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &libstd_stamp(builder, compiler, target),
                  false);

        builder.ensure(StdLink {
            compiler: builder.compiler(compiler.stage, builder.config.build),
            target_compiler: compiler,
            target,
        });
    }
}

/// Copies the crt(1,i,n).o startup objects
///
/// Since musl supports fully static linking, we can cross link for it even
/// with a glibc-targeting toolchain, given we have the appropriate startup
/// files. As those shipped with glibc won't work, copy the ones provided by
/// musl so we have them on linux-gnu hosts.
fn copy_musl_third_party_objects(builder: &Builder,
                                 target: Interned<String>,
                                 into: &Path) {
    for &obj in &["crt1.o", "crti.o", "crtn.o"] {
        builder.copy(&builder.musl_root(target).unwrap().join("lib").join(obj), &into.join(obj));
    }
}

/// Configure cargo to compile the standard library, adding appropriate env vars
/// and such.
pub fn std_cargo(builder: &Builder,
                 compiler: &Compiler,
                 target: Interned<String>,
                 cargo: &mut Command) {
    if let Some(target) = env::var_os("MACOSX_STD_DEPLOYMENT_TARGET") {
        cargo.env("MACOSX_DEPLOYMENT_TARGET", target);
    }

    if builder.no_std(target) == Some(true) {
        // for no-std targets we only compile a few no_std crates
        cargo.arg("--features").arg("c mem")
            .args(&["-p", "alloc"])
            .args(&["-p", "compiler_builtins"])
            .arg("--manifest-path")
            .arg(builder.src.join("src/rustc/compiler_builtins_shim/Cargo.toml"));
    } else {
        let mut features = builder.std_features();

        // When doing a local rebuild we tell cargo that we're stage1 rather than
        // stage0. This works fine if the local rust and being-built rust have the
        // same view of what the default allocator is, but fails otherwise. Since
        // we don't have a way to express an allocator preference yet, work
        // around the issue in the case of a local rebuild with jemalloc disabled.
        if compiler.stage == 0 && builder.local_rebuild && !builder.config.use_jemalloc {
            features.push_str(" force_alloc_system");
        }

        if compiler.stage != 0 && builder.config.sanitizers {
            // This variable is used by the sanitizer runtime crates, e.g.
            // rustc_lsan, to build the sanitizer runtime from C code
            // When this variable is missing, those crates won't compile the C code,
            // so we don't set this variable during stage0 where llvm-config is
            // missing
            // We also only build the runtimes when --enable-sanitizers (or its
            // config.toml equivalent) is used
            let llvm_config = builder.ensure(native::Llvm {
                target: builder.config.build,
                emscripten: false,
            });
            cargo.env("LLVM_CONFIG", llvm_config);
        }

        cargo.arg("--features").arg(features)
            .arg("--manifest-path")
            .arg(builder.src.join("src/libstd/Cargo.toml"));

        if let Some(target) = builder.config.target_config.get(&target) {
            if let Some(ref jemalloc) = target.jemalloc {
                cargo.env("JEMALLOC_OVERRIDE", jemalloc);
            }
        }
        if target.contains("musl") {
            if let Some(p) = builder.musl_root(target) {
                cargo.env("MUSL_ROOT", p);
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct StdLink {
    pub compiler: Compiler,
    pub target_compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for StdLink {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Link all libstd rlibs/dylibs into the sysroot location.
    ///
    /// Links those artifacts generated by `compiler` to a the `stage` compiler's
    /// sysroot for the specified `host` and `target`.
    ///
    /// Note that this assumes that `compiler` has already generated the libstd
    /// libraries for `target`, and this method will find them in the relevant
    /// output directory.
    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target_compiler = self.target_compiler;
        let target = self.target;
        builder.info(&format!("Copying stage{} std from stage{} ({} -> {} / {})",
                target_compiler.stage,
                compiler.stage,
                &compiler.host,
                target_compiler.host,
                target));
        let libdir = builder.sysroot_libdir(target_compiler, target);
        add_to_sysroot(builder, &libdir, &libstd_stamp(builder, compiler, target));

        if builder.config.sanitizers && compiler.stage != 0 && target == "x86_64-apple-darwin" {
            // The sanitizers are only built in stage1 or above, so the dylibs will
            // be missing in stage0 and causes panic. See the `std()` function above
            // for reason why the sanitizers are not built in stage0.
            copy_apple_sanitizer_dylibs(builder, &builder.native_dir(target), "osx", &libdir);
        }

        builder.ensure(tool::CleanTools {
            compiler: target_compiler,
            target,
            cause: Mode::Std,
        });
    }
}

fn copy_apple_sanitizer_dylibs(builder: &Builder, native_dir: &Path, platform: &str, into: &Path) {
    for &sanitizer in &["asan", "tsan"] {
        let filename = format!("libclang_rt.{}_{}_dynamic.dylib", sanitizer, platform);
        let mut src_path = native_dir.join(sanitizer);
        src_path.push("build");
        src_path.push("lib");
        src_path.push("darwin");
        src_path.push(&filename);
        builder.copy(&src_path, &into.join(filename));
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct StartupObjects {
    pub compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for StartupObjects {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/rtstartup")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(StartupObjects {
            compiler: run.builder.compiler(run.builder.top_stage, run.host),
            target: run.target,
        });
    }

    /// Build and prepare startup objects like rsbegin.o and rsend.o
    ///
    /// These are primarily used on Windows right now for linking executables/dlls.
    /// They don't require any library support as they're just plain old object
    /// files, so we just use the nightly snapshot compiler to always build them (as
    /// no other compilers are guaranteed to be available).
    fn run(self, builder: &Builder) {
        let for_compiler = self.compiler;
        let target = self.target;
        if !target.contains("pc-windows-gnu") {
            return
        }

        let src_dir = &builder.src.join("src/rtstartup");
        let dst_dir = &builder.native_dir(target).join("rtstartup");
        let sysroot_dir = &builder.sysroot_libdir(for_compiler, target);
        t!(fs::create_dir_all(dst_dir));

        for file in &["rsbegin", "rsend"] {
            let src_file = &src_dir.join(file.to_string() + ".rs");
            let dst_file = &dst_dir.join(file.to_string() + ".o");
            if !up_to_date(src_file, dst_file) {
                let mut cmd = Command::new(&builder.initial_rustc);
                builder.run(cmd.env("RUSTC_BOOTSTRAP", "1")
                            .arg("--cfg").arg("stage0")
                            .arg("--target").arg(target)
                            .arg("--emit=obj")
                            .arg("-o").arg(dst_file)
                            .arg(src_file));
            }

            builder.copy(dst_file, &sysroot_dir.join(file.to_string() + ".o"));
        }

        for obj in ["crt2.o", "dllcrt2.o"].iter() {
            let src = compiler_file(builder,
                                    builder.cc(target),
                                    target,
                                    obj);
            builder.copy(&src, &sysroot_dir.join(obj));
        }
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Test {
    pub target: Interned<String>,
    pub compiler: Compiler,
}

impl Step for Test {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("test")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Test {
            compiler: run.builder.compiler(run.builder.top_stage, run.host),
            target: run.target,
        });
    }

    /// Build libtest.
    ///
    /// This will build libtest and supporting libraries for a particular stage of
    /// the build using the `compiler` targeting the `target` architecture. The
    /// artifacts created will also be linked into the sysroot directory.
    fn run(self, builder: &Builder) {
        let target = self.target;
        let compiler = self.compiler;

        builder.ensure(Std { compiler, target });

        if builder.config.keep_stage.contains(&compiler.stage) {
            builder.info("Warning: Using a potentially old libtest. This may not behave well.");
            builder.ensure(TestLink {
                compiler: compiler,
                target_compiler: compiler,
                target,
            });
            return;
        }

        if builder.force_use_stage1(compiler, target) {
            builder.ensure(Test {
                compiler: builder.compiler(1, builder.config.build),
                target,
            });
            builder.info(
                &format!("Uplifting stage1 test ({} -> {})", builder.config.build, target));
            builder.ensure(TestLink {
                compiler: builder.compiler(1, builder.config.build),
                target_compiler: compiler,
                target,
            });
            return;
        }

        let out_dir = builder.cargo_out(compiler, Mode::Test, target);
        builder.clear_if_dirty(&out_dir, &libstd_stamp(builder, compiler, target));
        let mut cargo = builder.cargo(compiler, Mode::Test, target, "build");
        test_cargo(builder, &compiler, target, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-test", compiler.stage));
        builder.info(&format!("Building stage{} test artifacts ({} -> {})", compiler.stage,
                &compiler.host, target));
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &libtest_stamp(builder, compiler, target),
                  false);

        builder.ensure(TestLink {
            compiler: builder.compiler(compiler.stage, builder.config.build),
            target_compiler: compiler,
            target,
        });
    }
}

/// Same as `std_cargo`, but for libtest
pub fn test_cargo(builder: &Builder,
                  _compiler: &Compiler,
                  _target: Interned<String>,
                  cargo: &mut Command) {
    if let Some(target) = env::var_os("MACOSX_STD_DEPLOYMENT_TARGET") {
        cargo.env("MACOSX_DEPLOYMENT_TARGET", target);
    }
    cargo.arg("--manifest-path")
        .arg(builder.src.join("src/libtest/Cargo.toml"));
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TestLink {
    pub compiler: Compiler,
    pub target_compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for TestLink {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Same as `std_link`, only for libtest
    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target_compiler = self.target_compiler;
        let target = self.target;
        builder.info(&format!("Copying stage{} test from stage{} ({} -> {} / {})",
                target_compiler.stage,
                compiler.stage,
                &compiler.host,
                target_compiler.host,
                target));
        add_to_sysroot(builder, &builder.sysroot_libdir(target_compiler, target),
                    &libtest_stamp(builder, compiler, target));
        builder.ensure(tool::CleanTools {
            compiler: target_compiler,
            target,
            cause: Mode::Test,
        });
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Rustc {
    pub target: Interned<String>,
    pub compiler: Compiler,
}

impl Step for Rustc {
    type Output = ();
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("rustc-main")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustc {
            compiler: run.builder.compiler(run.builder.top_stage, run.host),
            target: run.target,
        });
    }

    /// Build the compiler.
    ///
    /// This will build the compiler for a particular stage of the build using
    /// the `compiler` targeting the `target` architecture. The artifacts
    /// created will also be linked into the sysroot directory.
    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target = self.target;

        builder.ensure(Test { compiler, target });

        if builder.config.keep_stage.contains(&compiler.stage) {
            builder.info("Warning: Using a potentially old librustc. This may not behave well.");
            builder.ensure(RustcLink {
                compiler: compiler,
                target_compiler: compiler,
                target,
            });
            return;
        }

        if builder.force_use_stage1(compiler, target) {
            builder.ensure(Rustc {
                compiler: builder.compiler(1, builder.config.build),
                target,
            });
            builder.info(&format!("Uplifting stage1 rustc ({} -> {})",
                builder.config.build, target));
            builder.ensure(RustcLink {
                compiler: builder.compiler(1, builder.config.build),
                target_compiler: compiler,
                target,
            });
            return;
        }

        // Ensure that build scripts have a std to link against.
        builder.ensure(Std {
            compiler: builder.compiler(self.compiler.stage, builder.config.build),
            target: builder.config.build,
        });
        let cargo_out = builder.cargo_out(compiler, Mode::Rustc, target);
        builder.clear_if_dirty(&cargo_out, &libstd_stamp(builder, compiler, target));
        builder.clear_if_dirty(&cargo_out, &libtest_stamp(builder, compiler, target));

        let mut cargo = builder.cargo(compiler, Mode::Rustc, target, "build");
        rustc_cargo(builder, &mut cargo);

        let _folder = builder.fold_output(|| format!("stage{}-rustc", compiler.stage));
        builder.info(&format!("Building stage{} compiler artifacts ({} -> {})",
                 compiler.stage, &compiler.host, target));
        run_cargo(builder,
                  &mut cargo,
                  vec![],
                  &librustc_stamp(builder, compiler, target),
                  false);

        builder.ensure(RustcLink {
            compiler: builder.compiler(compiler.stage, builder.config.build),
            target_compiler: compiler,
            target,
        });
    }
}

pub fn rustc_cargo(builder: &Builder, cargo: &mut Command) {
    cargo.arg("--features").arg(builder.rustc_features())
         .arg("--manifest-path")
         .arg(builder.src.join("src/rustc/Cargo.toml"));
    rustc_cargo_env(builder, cargo);
}

pub fn rustc_cargo_env(builder: &Builder, cargo: &mut Command) {
    // Set some configuration variables picked up by build scripts and
    // the compiler alike
    cargo.env("CFG_RELEASE", builder.rust_release())
         .env("CFG_RELEASE_CHANNEL", &builder.config.channel)
         .env("CFG_VERSION", builder.rust_version())
         .env("CFG_PREFIX", builder.config.prefix.clone().unwrap_or_default())
         .env("CFG_CODEGEN_BACKENDS_DIR", &builder.config.rust_codegen_backends_dir);

    let libdir_relative = builder.config.libdir_relative().unwrap_or(Path::new("lib"));
    cargo.env("CFG_LIBDIR_RELATIVE", libdir_relative);

    // If we're not building a compiler with debugging information then remove
    // these two env vars which would be set otherwise.
    if builder.config.rust_debuginfo_only_std {
        cargo.env_remove("RUSTC_DEBUGINFO");
        cargo.env_remove("RUSTC_DEBUGINFO_LINES");
    }

    if let Some(ref ver_date) = builder.rust_info.commit_date() {
        cargo.env("CFG_VER_DATE", ver_date);
    }
    if let Some(ref ver_hash) = builder.rust_info.sha() {
        cargo.env("CFG_VER_HASH", ver_hash);
    }
    if !builder.unstable_features() {
        cargo.env("CFG_DISABLE_UNSTABLE_FEATURES", "1");
    }
    if let Some(ref s) = builder.config.rustc_default_linker {
        cargo.env("CFG_DEFAULT_LINKER", s);
    }
    if builder.config.rustc_parallel_queries {
        cargo.env("RUSTC_PARALLEL_QUERIES", "1");
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct RustcLink {
    pub compiler: Compiler,
    pub target_compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for RustcLink {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Same as `std_link`, only for librustc
    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target_compiler = self.target_compiler;
        let target = self.target;
        builder.info(&format!("Copying stage{} rustc from stage{} ({} -> {} / {})",
                 target_compiler.stage,
                 compiler.stage,
                 &compiler.host,
                 target_compiler.host,
                 target));
        add_to_sysroot(builder, &builder.sysroot_libdir(target_compiler, target),
                       &librustc_stamp(builder, compiler, target));
        builder.ensure(tool::CleanTools {
            compiler: target_compiler,
            target,
            cause: Mode::Rustc,
        });
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct CodegenBackend {
    pub compiler: Compiler,
    pub target: Interned<String>,
    pub backend: Interned<String>,
}

impl Step for CodegenBackend {
    type Output = ();
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.all_krates("rustc_codegen_llvm")
    }

    fn make_run(run: RunConfig) {
        let backend = run.builder.config.rust_codegen_backends.get(0);
        let backend = backend.cloned().unwrap_or_else(|| {
            INTERNER.intern_str("llvm")
        });
        run.builder.ensure(CodegenBackend {
            compiler: run.builder.compiler(run.builder.top_stage, run.host),
            target: run.target,
            backend,
        });
    }

    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target = self.target;
        let backend = self.backend;

        builder.ensure(Rustc { compiler, target });

        if builder.config.keep_stage.contains(&compiler.stage) {
            builder.info("Warning: Using a potentially old codegen backend. \
                This may not behave well.");
            // Codegen backends are linked separately from this step today, so we don't do
            // anything here.
            return;
        }

        if builder.force_use_stage1(compiler, target) {
            builder.ensure(CodegenBackend {
                compiler: builder.compiler(1, builder.config.build),
                target,
                backend,
            });
            return;
        }

        let out_dir = builder.cargo_out(compiler, Mode::Codegen, target);
        builder.clear_if_dirty(&out_dir, &librustc_stamp(builder, compiler, target));

        let mut cargo = builder.cargo(compiler, Mode::Codegen, target, "rustc");
        cargo.arg("--manifest-path")
            .arg(builder.src.join("src/librustc_codegen_llvm/Cargo.toml"));
        rustc_cargo_env(builder, &mut cargo);

        let features = build_codegen_backend(&builder, &mut cargo, &compiler, target, backend);

        let mut cargo_tails_args = vec![];

        if builder.config.llvm_thin_lto {
            cargo_tails_args.push("--".to_string());

            let num_jobs = builder.jobs();

            if !target.contains("msvc") {
                // Here we assume that the linker is clang. If it's not, there'll
                // be linker errors.
                cargo_tails_args.push("-Clink-arg=-fuse-ld=lld".to_string());
                cargo_tails_args.push("-Clink-arg=-flto=thin".to_string());

                if builder.config.llvm_optimize {
                    cargo_tails_args.push("-Clink-arg=-O2".to_string());
                }

                // Let's make LLD respect the `-j` option.
                let num_jobs_arg = format!("-Clink-arg=-Wl,--thinlto-jobs={}", num_jobs);
                cargo_tails_args.push(num_jobs_arg);
            } else {
                // Here we assume that the linker is lld-link.exe. lld-link.exe
                // does not need the extra arguments except for num_jobs
                let num_jobs_arg = format!("-Clink-arg=/opt:lldltojobs={}", num_jobs);
                cargo_tails_args.push(num_jobs_arg);
            }
        }

        let tmp_stamp = out_dir.join(".tmp.stamp");

        let _folder = builder.fold_output(|| format!("stage{}-rustc_codegen_llvm", compiler.stage));
        let files = run_cargo(builder,
                              cargo.arg("--features").arg(features),
                              cargo_tails_args,
                              &tmp_stamp,
                              false);
        if builder.config.dry_run {
            return;
        }
        let mut files = files.into_iter()
            .filter(|f| {
                let filename = f.file_name().unwrap().to_str().unwrap();
                is_dylib(filename) && filename.contains("rustc_codegen_llvm-")
            });
        let codegen_backend = match files.next() {
            Some(f) => f,
            None => panic!("no dylibs built for codegen backend?"),
        };
        if let Some(f) = files.next() {
            panic!("codegen backend built two dylibs:\n{}\n{}",
                   codegen_backend.display(),
                   f.display());
        }
        let stamp = codegen_backend_stamp(builder, compiler, target, backend);
        let codegen_backend = codegen_backend.to_str().unwrap();
        t!(t!(File::create(&stamp)).write_all(codegen_backend.as_bytes()));
    }
}

pub fn build_codegen_backend(builder: &Builder,
                             cargo: &mut Command,
                             compiler: &Compiler,
                             target: Interned<String>,
                             backend: Interned<String>) -> String {
    let mut features = String::new();

    match &*backend {
        "llvm" | "emscripten" => {
            // Build LLVM for our target. This will implicitly build the
            // host LLVM if necessary.
            let llvm_config = builder.ensure(native::Llvm {
                target,
                emscripten: backend == "emscripten",
            });

            if backend == "emscripten" {
                features.push_str(" emscripten");
            }

            builder.info(&format!("Building stage{} codegen artifacts ({} -> {}, {})",
                     compiler.stage, &compiler.host, target, backend));

            // Pass down configuration from the LLVM build into the build of
            // librustc_llvm and librustc_codegen_llvm.
            if builder.is_rust_llvm(target) {
                cargo.env("LLVM_RUSTLLVM", "1");
            }
            cargo.env("LLVM_CONFIG", &llvm_config);
            if backend != "emscripten" {
                let target_config = builder.config.target_config.get(&target);
                if let Some(s) = target_config.and_then(|c| c.llvm_config.as_ref()) {
                    cargo.env("CFG_LLVM_ROOT", s);
                }
            }
            // Building with a static libstdc++ is only supported on linux right now,
            // not for MSVC or macOS
            if builder.config.llvm_static_stdcpp &&
               !target.contains("freebsd") &&
               !target.contains("windows") &&
               !target.contains("apple") {
                let file = compiler_file(builder,
                                         builder.cxx(target).unwrap(),
                                         target,
                                         "libstdc++.a");
                cargo.env("LLVM_STATIC_STDCPP", file);
            }
            if builder.config.llvm_link_shared {
                cargo.env("LLVM_LINK_SHARED", "1");
            }
        }
        _ => panic!("unknown backend: {}", backend),
    }

    features
}

/// Creates the `codegen-backends` folder for a compiler that's about to be
/// assembled as a complete compiler.
///
/// This will take the codegen artifacts produced by `compiler` and link them
/// into an appropriate location for `target_compiler` to be a functional
/// compiler.
fn copy_codegen_backends_to_sysroot(builder: &Builder,
                                    compiler: Compiler,
                                    target_compiler: Compiler) {
    let target = target_compiler.host;

    // Note that this step is different than all the other `*Link` steps in
    // that it's not assembling a bunch of libraries but rather is primarily
    // moving the codegen backend into place. The codegen backend of rustc is
    // not linked into the main compiler by default but is rather dynamically
    // selected at runtime for inclusion.
    //
    // Here we're looking for the output dylib of the `CodegenBackend` step and
    // we're copying that into the `codegen-backends` folder.
    let dst = builder.sysroot_codegen_backends(target_compiler);
    t!(fs::create_dir_all(&dst));

    if builder.config.dry_run {
        return;
    }

    for backend in builder.config.rust_codegen_backends.iter() {
        let stamp = codegen_backend_stamp(builder, compiler, target, *backend);
        let mut dylib = String::new();
        t!(t!(File::open(&stamp)).read_to_string(&mut dylib));
        let file = Path::new(&dylib);
        let filename = file.file_name().unwrap().to_str().unwrap();
        // change `librustc_codegen_llvm-xxxxxx.so` to `librustc_codegen_llvm-llvm.so`
        let target_filename = {
            let dash = filename.find("-").unwrap();
            let dot = filename.find(".").unwrap();
            format!("{}-{}{}",
                    &filename[..dash],
                    backend,
                    &filename[dot..])
        };
        builder.copy(&file, &dst.join(target_filename));
    }
}

fn copy_lld_to_sysroot(builder: &Builder,
                       target_compiler: Compiler,
                       lld_install_root: &Path) {
    let target = target_compiler.host;

    let dst = builder.sysroot_libdir(target_compiler, target)
        .parent()
        .unwrap()
        .join("bin");
    t!(fs::create_dir_all(&dst));

    let src_exe = exe("lld", &target);
    let dst_exe = exe("rust-lld", &target);
    // we prepend this bin directory to the user PATH when linking Rust binaries. To
    // avoid shadowing the system LLD we rename the LLD we provide to `rust-lld`.
    builder.copy(&lld_install_root.join("bin").join(&src_exe), &dst.join(&dst_exe));
}

/// Cargo's output path for the standard library in a given stage, compiled
/// by a particular compiler for the specified target.
pub fn libstd_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Std, target).join(".libstd.stamp")
}

/// Cargo's output path for libtest in a given stage, compiled by a particular
/// compiler for the specified target.
pub fn libtest_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Test, target).join(".libtest.stamp")
}

/// Cargo's output path for librustc in a given stage, compiled by a particular
/// compiler for the specified target.
pub fn librustc_stamp(builder: &Builder, compiler: Compiler, target: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Rustc, target).join(".librustc.stamp")
}

/// Cargo's output path for librustc_codegen_llvm in a given stage, compiled by a particular
/// compiler for the specified target and backend.
fn codegen_backend_stamp(builder: &Builder,
                         compiler: Compiler,
                         target: Interned<String>,
                         backend: Interned<String>) -> PathBuf {
    builder.cargo_out(compiler, Mode::Codegen, target)
        .join(format!(".librustc_codegen_llvm-{}.stamp", backend))
}

pub fn compiler_file(builder: &Builder,
                 compiler: &Path,
                 target: Interned<String>,
                 file: &str) -> PathBuf {
    let mut cmd = Command::new(compiler);
    cmd.args(builder.cflags(target));
    cmd.arg(format!("-print-file-name={}", file));
    let out = output(&mut cmd);
    PathBuf::from(out.trim())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Sysroot {
    pub compiler: Compiler,
}

impl Step for Sysroot {
    type Output = Interned<PathBuf>;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Returns the sysroot for the `compiler` specified that *this build system
    /// generates*.
    ///
    /// That is, the sysroot for the stage0 compiler is not what the compiler
    /// thinks it is by default, but it's the same as the default for stages
    /// 1-3.
    fn run(self, builder: &Builder) -> Interned<PathBuf> {
        let compiler = self.compiler;
        let sysroot = if compiler.stage == 0 {
            builder.out.join(&compiler.host).join("stage0-sysroot")
        } else {
            builder.out.join(&compiler.host).join(format!("stage{}", compiler.stage))
        };
        let _ = fs::remove_dir_all(&sysroot);
        t!(fs::create_dir_all(&sysroot));
        INTERNER.intern_path(sysroot)
    }
}

#[derive(Debug, Copy, PartialOrd, Ord, Clone, PartialEq, Eq, Hash)]
pub struct Assemble {
    /// The compiler which we will produce in this step. Assemble itself will
    /// take care of ensuring that the necessary prerequisites to do so exist,
    /// that is, this target can be a stage2 compiler and Assemble will build
    /// previous stages for you.
    pub target_compiler: Compiler,
}

impl Step for Assemble {
    type Output = Compiler;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Prepare a new compiler from the artifacts in `stage`
    ///
    /// This will assemble a compiler in `build/$host/stage$stage`. The compiler
    /// must have been previously produced by the `stage - 1` builder.build
    /// compiler.
    fn run(self, builder: &Builder) -> Compiler {
        let target_compiler = self.target_compiler;

        if target_compiler.stage == 0 {
            assert_eq!(builder.config.build, target_compiler.host,
                "Cannot obtain compiler for non-native build triple at stage 0");
            // The stage 0 compiler for the build triple is always pre-built.
            return target_compiler;
        }

        // Get the compiler that we'll use to bootstrap ourselves.
        //
        // Note that this is where the recursive nature of the bootstrap
        // happens, as this will request the previous stage's compiler on
        // downwards to stage 0.
        //
        // Also note that we're building a compiler for the host platform. We
        // only assume that we can run `build` artifacts, which means that to
        // produce some other architecture compiler we need to start from
        // `build` to get there.
        //
        // FIXME: Perhaps we should download those libraries?
        //        It would make builds faster...
        //
        // FIXME: It may be faster if we build just a stage 1 compiler and then
        //        use that to bootstrap this compiler forward.
        let build_compiler =
            builder.compiler(target_compiler.stage - 1, builder.config.build);

        // Build the libraries for this compiler to link to (i.e., the libraries
        // it uses at runtime). NOTE: Crates the target compiler compiles don't
        // link to these. (FIXME: Is that correct? It seems to be correct most
        // of the time but I think we do link to these for stage2/bin compilers
        // when not performing a full bootstrap).
        builder.ensure(Rustc {
            compiler: build_compiler,
            target: target_compiler.host,
        });
        for &backend in builder.config.rust_codegen_backends.iter() {
            builder.ensure(CodegenBackend {
                compiler: build_compiler,
                target: target_compiler.host,
                backend,
            });
        }

        let lld_install = if builder.config.lld_enabled {
            Some(builder.ensure(native::Lld {
                target: target_compiler.host,
            }))
        } else {
            None
        };

        let stage = target_compiler.stage;
        let host = target_compiler.host;
        builder.info(&format!("Assembling stage{} compiler ({})", stage, host));

        // Link in all dylibs to the libdir
        let sysroot = builder.sysroot(target_compiler);
        let sysroot_libdir = sysroot.join(libdir(&*host));
        t!(fs::create_dir_all(&sysroot_libdir));
        let src_libdir = builder.sysroot_libdir(build_compiler, host);
        for f in builder.read_dir(&src_libdir) {
            let filename = f.file_name().into_string().unwrap();
            if is_dylib(&filename) {
                builder.copy(&f.path(), &sysroot_libdir.join(&filename));
            }
        }

        copy_codegen_backends_to_sysroot(builder,
                                         build_compiler,
                                         target_compiler);
        if let Some(lld_install) = lld_install {
            copy_lld_to_sysroot(builder, target_compiler, &lld_install);
        }

        // Link the compiler binary itself into place
        let out_dir = builder.cargo_out(build_compiler, Mode::Rustc, host);
        let rustc = out_dir.join(exe("rustc_binary", &*host));
        let bindir = sysroot.join("bin");
        t!(fs::create_dir_all(&bindir));
        let compiler = builder.rustc(target_compiler);
        let _ = fs::remove_file(&compiler);
        builder.copy(&rustc, &compiler);

        target_compiler
    }
}

/// Link some files into a rustc sysroot.
///
/// For a particular stage this will link the file listed in `stamp` into the
/// `sysroot_dst` provided.
pub fn add_to_sysroot(builder: &Builder, sysroot_dst: &Path, stamp: &Path) {
    t!(fs::create_dir_all(&sysroot_dst));
    for path in builder.read_stamp_file(stamp) {
        builder.copy(&path, &sysroot_dst.join(path.file_name().unwrap()));
    }
}

// Avoiding a dependency on winapi to keep compile times down
#[cfg(unix)]
fn stderr_isatty() -> bool {
    use libc;
    unsafe { libc::isatty(libc::STDERR_FILENO) != 0 }
}
#[cfg(windows)]
fn stderr_isatty() -> bool {
    type DWORD = u32;
    type BOOL = i32;
    type HANDLE = *mut u8;
    const STD_ERROR_HANDLE: DWORD = -12i32 as DWORD;
    extern "system" {
        fn GetStdHandle(which: DWORD) -> HANDLE;
        fn GetConsoleMode(hConsoleHandle: HANDLE, lpMode: *mut DWORD) -> BOOL;
    }
    unsafe {
        let handle = GetStdHandle(STD_ERROR_HANDLE);
        let mut out = 0;
        GetConsoleMode(handle, &mut out) != 0
    }
}

pub fn run_cargo(builder: &Builder,
                 cargo: &mut Command,
                 tail_args: Vec<String>,
                 stamp: &Path,
                 is_check: bool)
    -> Vec<PathBuf>
{
    if builder.config.dry_run {
        return Vec::new();
    }

    // `target_root_dir` looks like $dir/$target/release
    let target_root_dir = stamp.parent().unwrap();
    // `target_deps_dir` looks like $dir/$target/release/deps
    let target_deps_dir = target_root_dir.join("deps");
    // `host_root_dir` looks like $dir/release
    let host_root_dir = target_root_dir.parent().unwrap() // chop off `release`
                                       .parent().unwrap() // chop off `$target`
                                       .join(target_root_dir.file_name().unwrap());

    // Spawn Cargo slurping up its JSON output. We'll start building up the
    // `deps` array of all files it generated along with a `toplevel` array of
    // files we need to probe for later.
    let mut deps = Vec::new();
    let mut toplevel = Vec::new();
    let ok = stream_cargo(builder, cargo, tail_args, &mut |msg| {
        let filenames = match msg {
            CargoMessage::CompilerArtifact { filenames, .. } => filenames,
            _ => return,
        };
        for filename in filenames {
            // Skip files like executables
            if !filename.ends_with(".rlib") &&
               !filename.ends_with(".lib") &&
               !is_dylib(&filename) &&
               !(is_check && filename.ends_with(".rmeta")) {
                continue;
            }

            let filename = Path::new(&*filename);

            // If this was an output file in the "host dir" we don't actually
            // worry about it, it's not relevant for us.
            if filename.starts_with(&host_root_dir) {
                continue;
            }

            // If this was output in the `deps` dir then this is a precise file
            // name (hash included) so we start tracking it.
            if filename.starts_with(&target_deps_dir) {
                deps.push(filename.to_path_buf());
                continue;
            }

            // Otherwise this was a "top level artifact" which right now doesn't
            // have a hash in the name, but there's a version of this file in
            // the `deps` folder which *does* have a hash in the name. That's
            // the one we'll want to we'll probe for it later.
            //
            // We do not use `Path::file_stem` or `Path::extension` here,
            // because some generated files may have multiple extensions e.g.
            // `std-<hash>.dll.lib` on Windows. The aforementioned methods only
            // split the file name by the last extension (`.lib`) while we need
            // to split by all extensions (`.dll.lib`).
            let expected_len = t!(filename.metadata()).len();
            let filename = filename.file_name().unwrap().to_str().unwrap();
            let mut parts = filename.splitn(2, '.');
            let file_stem = parts.next().unwrap().to_owned();
            let extension = parts.next().unwrap().to_owned();

            toplevel.push((file_stem, extension, expected_len));
        }
    });

    if !ok {
        panic!("cargo must succeed");
    }

    // Ok now we need to actually find all the files listed in `toplevel`. We've
    // got a list of prefix/extensions and we basically just need to find the
    // most recent file in the `deps` folder corresponding to each one.
    let contents = t!(target_deps_dir.read_dir())
        .map(|e| t!(e))
        .map(|e| (e.path(), e.file_name().into_string().unwrap(), t!(e.metadata())))
        .collect::<Vec<_>>();
    for (prefix, extension, expected_len) in toplevel {
        let candidates = contents.iter().filter(|&&(_, ref filename, ref meta)| {
            filename.starts_with(&prefix[..]) &&
                filename[prefix.len()..].starts_with("-") &&
                filename.ends_with(&extension[..]) &&
                meta.len() == expected_len
        });
        let max = candidates.max_by_key(|&&(_, _, ref metadata)| {
            FileTime::from_last_modification_time(metadata)
        });
        let path_to_add = match max {
            Some(triple) => triple.0.to_str().unwrap(),
            None => panic!("no output generated for {:?} {:?}", prefix, extension),
        };
        if is_dylib(path_to_add) {
            let candidate = format!("{}.lib", path_to_add);
            let candidate = PathBuf::from(candidate);
            if candidate.exists() {
                deps.push(candidate);
            }
        }
        deps.push(path_to_add.into());
    }

    // Now we want to update the contents of the stamp file, if necessary. First
    // we read off the previous contents along with its mtime. If our new
    // contents (the list of files to copy) is different or if any dep's mtime
    // is newer then we rewrite the stamp file.
    deps.sort();
    let mut stamp_contents = Vec::new();
    if let Ok(mut f) = File::open(stamp) {
        t!(f.read_to_end(&mut stamp_contents));
    }
    let stamp_mtime = mtime(&stamp);
    let mut new_contents = Vec::new();
    let mut max = None;
    let mut max_path = None;
    for dep in deps.iter() {
        let mtime = mtime(dep);
        if Some(mtime) > max {
            max = Some(mtime);
            max_path = Some(dep.clone());
        }
        new_contents.extend(dep.to_str().unwrap().as_bytes());
        new_contents.extend(b"\0");
    }
    let max = max.unwrap();
    let max_path = max_path.unwrap();
    if stamp_contents == new_contents && max <= stamp_mtime {
        builder.verbose(&format!("not updating {:?}; contents equal and {:?} <= {:?}",
                stamp, max, stamp_mtime));
        return deps
    }
    if max > stamp_mtime {
        builder.verbose(&format!("updating {:?} as {:?} changed", stamp, max_path));
    } else {
        builder.verbose(&format!("updating {:?} as deps changed", stamp));
    }
    t!(t!(File::create(stamp)).write_all(&new_contents));
    deps
}

pub fn stream_cargo(
    builder: &Builder,
    cargo: &mut Command,
    tail_args: Vec<String>,
    cb: &mut dyn FnMut(CargoMessage),
) -> bool {
    if builder.config.dry_run {
        return true;
    }
    // Instruct Cargo to give us json messages on stdout, critically leaving
    // stderr as piped so we can get those pretty colors.
    cargo.arg("--message-format").arg("json")
         .stdout(Stdio::piped());

    if stderr_isatty() && builder.ci_env == CiEnv::None &&
        // if the terminal is reported as dumb, then we don't want to enable color for rustc
        env::var_os("TERM").map(|t| t != *"dumb").unwrap_or(true) {
        // since we pass message-format=json to cargo, we need to tell the rustc
        // wrapper to give us colored output if necessary. This is because we
        // only want Cargo's JSON output, not rustcs.
        cargo.env("RUSTC_COLOR", "1");
    }

    for arg in tail_args {
        cargo.arg(arg);
    }

    builder.verbose(&format!("running: {:?}", cargo));
    let mut child = match cargo.spawn() {
        Ok(child) => child,
        Err(e) => panic!("failed to execute command: {:?}\nerror: {}", cargo, e),
    };

    // Spawn Cargo slurping up its JSON output. We'll start building up the
    // `deps` array of all files it generated along with a `toplevel` array of
    // files we need to probe for later.
    let stdout = BufReader::new(child.stdout.take().unwrap());
    for line in stdout.lines() {
        let line = t!(line);
        match serde_json::from_str::<CargoMessage>(&line) {
            Ok(msg) => cb(msg),
            // If this was informational, just print it out and continue
            Err(_) => println!("{}", line)
        }
    }

    // Make sure Cargo actually succeeded after we read all of its stdout.
    let status = t!(child.wait());
    if !status.success() {
        eprintln!("command did not execute successfully: {:?}\n\
                  expected success, got: {}",
                 cargo,
                 status);
    }
    status.success()
}

#[derive(Deserialize)]
#[serde(tag = "reason", rename_all = "kebab-case")]
pub enum CargoMessage<'a> {
    CompilerArtifact {
        package_id: Cow<'a, str>,
        features: Vec<Cow<'a, str>>,
        filenames: Vec<Cow<'a, str>>,
    },
    BuildScriptExecuted {
        package_id: Cow<'a, str>,
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Serialized configuration of a build.
//
// This module implements parsing `config.toml` configuration files to tweak
// how the build runs.

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process;
use std::cmp;

use num_cpus;
use toml;
use cache::{INTERNER, Interned};
use flags::Flags;
pub use flags::Subcommand;

/// Global configuration for the entire build and/or bootstrap.
///
/// This structure is derived from a combination of both `config.toml` and
/// `config.mk`. As of the time of this writing it's unlikely that `config.toml`
/// is used all that much, so this is primarily filled out by `config.mk` which
/// is generated from `./configure`.
///
/// Note that this structure is not decoded directly into, but rather it is
/// filled out from the decoded forms of the structs below. For documentation
/// each field, see the corresponding fields in
/// `config.toml.example`.
#[derive(Default)]
pub struct Config {
    pub ccache: Option<String>,
    pub ninja: bool,
    pub verbose: usize,
    pub submodules: bool,
    pub fast_submodules: bool,
    pub compiler_docs: bool,
    pub docs: bool,
    pub locked_deps: bool,
    pub vendor: bool,
    pub target_config: HashMap<Interned<String>, Target>,
    pub full_bootstrap: bool,
    pub extended: bool,
    pub tools: Option<HashSet<String>>,
    pub sanitizers: bool,
    pub profiler: bool,
    pub ignore_git: bool,
    pub exclude: Vec<PathBuf>,
    pub rustc_error_format: Option<String>,

    pub run_host_only: bool,

    pub on_fail: Option<String>,
    pub stage: Option<u32>,
    pub keep_stage: Vec<u32>,
    pub src: PathBuf,
    pub jobs: Option<u32>,
    pub cmd: Subcommand,
    pub incremental: bool,
    pub dry_run: bool,

    pub deny_warnings: bool,
    pub backtrace_on_ice: bool,

    // llvm codegen options
    pub llvm_enabled: bool,
    pub llvm_assertions: bool,
    pub llvm_optimize: bool,
    pub llvm_thin_lto: bool,
    pub llvm_release_debuginfo: bool,
    pub llvm_version_check: bool,
    pub llvm_static_stdcpp: bool,
    pub llvm_link_shared: bool,
    pub llvm_clang_cl: Option<String>,
    pub llvm_targets: Option<String>,
    pub llvm_experimental_targets: String,
    pub llvm_link_jobs: Option<u32>,

    pub lld_enabled: bool,
    pub lldb_enabled: bool,
    pub llvm_tools_enabled: bool,

    // rust codegen options
    pub rust_optimize: bool,
    pub rust_codegen_units: Option<u32>,
    pub rust_debug_assertions: bool,
    pub rust_debuginfo: bool,
    pub rust_debuginfo_lines: bool,
    pub rust_debuginfo_only_std: bool,
    pub rust_debuginfo_tools: bool,
    pub rust_rpath: bool,
    pub rustc_parallel_queries: bool,
    pub rustc_default_linker: Option<String>,
    pub rust_optimize_tests: bool,
    pub rust_debuginfo_tests: bool,
    pub rust_dist_src: bool,
    pub rust_codegen_backends: Vec<Interned<String>>,
    pub rust_codegen_backends_dir: String,
    pub rust_verify_llvm_ir: bool,

    pub build: Interned<String>,
    pub hosts: Vec<Interned<String>>,
    pub targets: Vec<Interned<String>>,
    pub local_rebuild: bool,

    // dist misc
    pub dist_sign_folder: Option<PathBuf>,
    pub dist_upload_addr: Option<String>,
    pub dist_gpg_password_file: Option<PathBuf>,

    // libstd features
    pub debug_jemalloc: bool,
    pub use_jemalloc: bool,
    pub backtrace: bool, // support for RUST_BACKTRACE
    pub wasm_syscall: bool,

    // misc
    pub low_priority: bool,
    pub channel: String,
    pub verbose_tests: bool,
    pub test_miri: bool,
    pub save_toolstates: Option<PathBuf>,
    pub print_step_timings: bool,

    // Fallback musl-root for all targets
    pub musl_root: Option<PathBuf>,
    pub prefix: Option<PathBuf>,
    pub sysconfdir: Option<PathBuf>,
    pub datadir: Option<PathBuf>,
    pub docdir: Option<PathBuf>,
    pub bindir: Option<PathBuf>,
    pub libdir: Option<PathBuf>,
    pub mandir: Option<PathBuf>,
    pub codegen_tests: bool,
    pub nodejs: Option<PathBuf>,
    pub gdb: Option<PathBuf>,
    pub python: Option<PathBuf>,
    pub openssl_static: bool,
    pub configure_args: Vec<String>,

    // These are either the stage0 downloaded binaries or the locally installed ones.
    pub initial_cargo: PathBuf,
    pub initial_rustc: PathBuf,
    pub out: PathBuf,
}

/// Per-target configuration stored in the global configuration structure.
#[derive(Default)]
pub struct Target {
    /// Some(path to llvm-config) if using an external LLVM.
    pub llvm_config: Option<PathBuf>,
    pub jemalloc: Option<PathBuf>,
    pub cc: Option<PathBuf>,
    pub cxx: Option<PathBuf>,
    pub ar: Option<PathBuf>,
    pub ranlib: Option<PathBuf>,
    pub linker: Option<PathBuf>,
    pub ndk: Option<PathBuf>,
    pub crt_static: Option<bool>,
    pub musl_root: Option<PathBuf>,
    pub qemu_rootfs: Option<PathBuf>,
    pub no_std: bool,
}

/// Structure of the `config.toml` file that configuration is read from.
///
/// This structure uses `Decodable` to automatically decode a TOML configuration
/// file into this format, and then this is traversed and written into the above
/// `Config` structure.
#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct TomlConfig {
    build: Option<Build>,
    install: Option<Install>,
    llvm: Option<Llvm>,
    rust: Option<Rust>,
    target: Option<HashMap<String, TomlTarget>>,
    dist: Option<Dist>,
}

/// TOML representation of various global build decisions.
#[derive(Deserialize, Default, Clone)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Build {
    build: Option<String>,
    #[serde(default)]
    host: Vec<String>,
    #[serde(default)]
    target: Vec<String>,
    cargo: Option<String>,
    rustc: Option<String>,
    low_priority: Option<bool>,
    compiler_docs: Option<bool>,
    docs: Option<bool>,
    submodules: Option<bool>,
    fast_submodules: Option<bool>,
    gdb: Option<String>,
    locked_deps: Option<bool>,
    vendor: Option<bool>,
    nodejs: Option<String>,
    python: Option<String>,
    full_bootstrap: Option<bool>,
    extended: Option<bool>,
    tools: Option<HashSet<String>>,
    verbose: Option<usize>,
    sanitizers: Option<bool>,
    profiler: Option<bool>,
    openssl_static: Option<bool>,
    configure_args: Option<Vec<String>>,
    local_rebuild: Option<bool>,
    print_step_timings: Option<bool>,
}

/// TOML representation of various global install decisions.
#[derive(Deserialize, Default, Clone)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Install {
    prefix: Option<String>,
    sysconfdir: Option<String>,
    datadir: Option<String>,
    docdir: Option<String>,
    bindir: Option<String>,
    libdir: Option<String>,
    mandir: Option<String>,

    // standard paths, currently unused
    infodir: Option<String>,
    localstatedir: Option<String>,
}

/// TOML representation of how the LLVM build is configured.
#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Llvm {
    enabled: Option<bool>,
    ccache: Option<StringOrBool>,
    ninja: Option<bool>,
    assertions: Option<bool>,
    optimize: Option<bool>,
    thin_lto: Option<bool>,
    release_debuginfo: Option<bool>,
    version_check: Option<bool>,
    static_libstdcpp: Option<bool>,
    targets: Option<String>,
    experimental_targets: Option<String>,
    link_jobs: Option<u32>,
    link_shared: Option<bool>,
    clang_cl: Option<String>
}

#[derive(Deserialize, Default, Clone)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Dist {
    sign_folder: Option<String>,
    gpg_password_file: Option<String>,
    upload_addr: Option<String>,
    src_tarball: Option<bool>,
}

#[derive(Deserialize)]
#[serde(untagged)]
enum StringOrBool {
    String(String),
    Bool(bool),
}

impl Default for StringOrBool {
    fn default() -> StringOrBool {
        StringOrBool::Bool(false)
    }
}

/// TOML representation of how the Rust build is configured.
#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct Rust {
    optimize: Option<bool>,
    codegen_units: Option<u32>,
    debug_assertions: Option<bool>,
    debuginfo: Option<bool>,
    debuginfo_lines: Option<bool>,
    debuginfo_only_std: Option<bool>,
    debuginfo_tools: Option<bool>,
    experimental_parallel_queries: Option<bool>,
    debug_jemalloc: Option<bool>,
    use_jemalloc: Option<bool>,
    backtrace: Option<bool>,
    default_linker: Option<String>,
    channel: Option<String>,
    musl_root: Option<String>,
    rpath: Option<bool>,
    optimize_tests: Option<bool>,
    debuginfo_tests: Option<bool>,
    codegen_tests: Option<bool>,
    ignore_git: Option<bool>,
    debug: Option<bool>,
    dist_src: Option<bool>,
    verbose_tests: Option<bool>,
    test_miri: Option<bool>,
    incremental: Option<bool>,
    save_toolstates: Option<String>,
    codegen_backends: Option<Vec<String>>,
    codegen_backends_dir: Option<String>,
    wasm_syscall: Option<bool>,
    lld: Option<bool>,
    lldb: Option<bool>,
    llvm_tools: Option<bool>,
    deny_warnings: Option<bool>,
    backtrace_on_ice: Option<bool>,
    verify_llvm_ir: Option<bool>,
}

/// TOML representation of how each build target is configured.
#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
struct TomlTarget {
    llvm_config: Option<String>,
    jemalloc: Option<String>,
    cc: Option<String>,
    cxx: Option<String>,
    ar: Option<String>,
    ranlib: Option<String>,
    linker: Option<String>,
    android_ndk: Option<String>,
    crt_static: Option<bool>,
    musl_root: Option<String>,
    qemu_rootfs: Option<String>,
}

impl Config {
    fn path_from_python(var_key: &str) -> PathBuf {
        match env::var_os(var_key) {
            // Do not trust paths from Python and normalize them slightly (#49785).
            Some(var_val) => Path::new(&var_val).components().collect(),
            _ => panic!("expected '{}' to be set", var_key),
        }
    }

    pub fn default_opts() -> Config {
        let mut config = Config::default();
        config.llvm_enabled = true;
        config.llvm_optimize = true;
        config.llvm_version_check = true;
        config.use_jemalloc = true;
        config.backtrace = true;
        config.rust_optimize = true;
        config.rust_optimize_tests = true;
        config.submodules = true;
        config.fast_submodules = true;
        config.docs = true;
        config.rust_rpath = true;
        config.channel = "dev".to_string();
        config.codegen_tests = true;
        config.ignore_git = false;
        config.rust_dist_src = true;
        config.test_miri = false;
        config.rust_codegen_backends = vec![INTERNER.intern_str("llvm")];
        config.rust_codegen_backends_dir = "codegen-backends".to_owned();
        config.deny_warnings = true;

        // set by bootstrap.py
        config.build = INTERNER.intern_str(&env::var("BUILD").expect("'BUILD' to be set"));
        config.src = Config::path_from_python("SRC");
        config.out = Config::path_from_python("BUILD_DIR");

        config.initial_rustc = Config::path_from_python("RUSTC");
        config.initial_cargo = Config::path_from_python("CARGO");

        config
    }

    pub fn parse(args: &[String]) -> Config {
        let flags = Flags::parse(&args);
        let file = flags.config.clone();
        let mut config = Config::default_opts();
        config.exclude = flags.exclude;
        config.rustc_error_format = flags.rustc_error_format;
        config.on_fail = flags.on_fail;
        config.stage = flags.stage;
        config.jobs = flags.jobs;
        config.cmd = flags.cmd;
        config.incremental = flags.incremental;
        config.dry_run = flags.dry_run;
        config.keep_stage = flags.keep_stage;
        if let Some(value) = flags.warnings {
            config.deny_warnings = value;
        }

        if config.dry_run {
            let dir = config.out.join("tmp-dry-run");
            t!(fs::create_dir_all(&dir));
            config.out = dir;
        }

        // If --target was specified but --host wasn't specified, don't run any host-only tests.
        config.run_host_only = !(flags.host.is_empty() && !flags.target.is_empty());

        let toml = file.map(|file| {
            let mut f = t!(File::open(&file));
            let mut contents = String::new();
            t!(f.read_to_string(&mut contents));
            match toml::from_str(&contents) {
                Ok(table) => table,
                Err(err) => {
                    println!("failed to parse TOML configuration '{}': {}",
                        file.display(), err);
                    process::exit(2);
                }
            }
        }).unwrap_or_else(|| TomlConfig::default());

        let build = toml.build.clone().unwrap_or(Build::default());
        // set by bootstrap.py
        config.hosts.push(config.build.clone());
        for host in build.host.iter() {
            let host = INTERNER.intern_str(host);
            if !config.hosts.contains(&host) {
                config.hosts.push(host);
            }
        }
        for target in config.hosts.iter().cloned()
            .chain(build.target.iter().map(|s| INTERNER.intern_str(s)))
        {
            if !config.targets.contains(&target) {
                config.targets.push(target);
            }
        }
        config.hosts = if !flags.host.is_empty() {
            flags.host
        } else {
            config.hosts
        };
        config.targets = if !flags.target.is_empty() {
            flags.target
        } else {
            config.targets
        };


        config.nodejs = build.nodejs.map(PathBuf::from);
        config.gdb = build.gdb.map(PathBuf::from);
        config.python = build.python.map(PathBuf::from);
        set(&mut config.low_priority, build.low_priority);
        set(&mut config.compiler_docs, build.compiler_docs);
        set(&mut config.docs, build.docs);
        set(&mut config.submodules, build.submodules);
        set(&mut config.fast_submodules, build.fast_submodules);
        set(&mut config.locked_deps, build.locked_deps);
        set(&mut config.vendor, build.vendor);
        set(&mut config.full_bootstrap, build.full_bootstrap);
        set(&mut config.extended, build.extended);
        config.tools = build.tools;
        set(&mut config.verbose, build.verbose);
        set(&mut config.sanitizers, build.sanitizers);
        set(&mut config.profiler, build.profiler);
        set(&mut config.openssl_static, build.openssl_static);
        set(&mut config.configure_args, build.configure_args);
        set(&mut config.local_rebuild, build.local_rebuild);
        set(&mut config.print_step_timings, build.print_step_timings);
        config.verbose = cmp::max(config.verbose, flags.verbose);

        if let Some(ref install) = toml.install {
            config.prefix = install.prefix.clone().map(PathBuf::from);
            config.sysconfdir = install.sysconfdir.clone().map(PathBuf::from);
            config.datadir = install.datadir.clone().map(PathBuf::from);
            config.docdir = install.docdir.clone().map(PathBuf::from);
            config.bindir = install.bindir.clone().map(PathBuf::from);
            config.libdir = install.libdir.clone().map(PathBuf::from);
            config.mandir = install.mandir.clone().map(PathBuf::from);
        }

        // Store off these values as options because if they're not provided
        // we'll infer default values for them later
        let mut llvm_assertions = None;
        let mut debuginfo_lines = None;
        let mut debuginfo_only_std = None;
        let mut debuginfo_tools = None;
        let mut debug = None;
        let mut debug_jemalloc = None;
        let mut debuginfo = None;
        let mut debug_assertions = None;
        let mut optimize = None;
        let mut ignore_git = None;

        if let Some(ref llvm) = toml.llvm {
            match llvm.ccache {
                Some(StringOrBool::String(ref s)) => {
                    config.ccache = Some(s.to_string())
                }
                Some(StringOrBool::Bool(true)) => {
                    config.ccache = Some("ccache".to_string());
                }
                Some(StringOrBool::Bool(false)) | None => {}
            }
            set(&mut config.ninja, llvm.ninja);
            set(&mut config.llvm_enabled, llvm.enabled);
            llvm_assertions = llvm.assertions;
            set(&mut config.llvm_optimize, llvm.optimize);
            set(&mut config.llvm_thin_lto, llvm.thin_lto);
            set(&mut config.llvm_release_debuginfo, llvm.release_debuginfo);
            set(&mut config.llvm_version_check, llvm.version_check);
            set(&mut config.llvm_static_stdcpp, llvm.static_libstdcpp);
            set(&mut config.llvm_link_shared, llvm.link_shared);
            config.llvm_targets = llvm.targets.clone();
            config.llvm_experimental_targets = llvm.experimental_targets.clone()
                .unwrap_or("WebAssembly;RISCV".to_string());
            config.llvm_link_jobs = llvm.link_jobs;
            config.llvm_clang_cl = llvm.clang_cl.clone();
        }

        if let Some(ref rust) = toml.rust {
            debug = rust.debug;
            debug_assertions = rust.debug_assertions;
            debuginfo = rust.debuginfo;
            debuginfo_lines = rust.debuginfo_lines;
            debuginfo_only_std = rust.debuginfo_only_std;
            debuginfo_tools = rust.debuginfo_tools;
            optimize = rust.optimize;
            ignore_git = rust.ignore_git;
            debug_jemalloc = rust.debug_jemalloc;
            set(&mut config.rust_optimize_tests, rust.optimize_tests);
            set(&mut config.rust_debuginfo_tests, rust.debuginfo_tests);
            set(&mut config.codegen_tests, rust.codegen_tests);
            set(&mut config.rust_rpath, rust.rpath);
            set(&mut config.use_jemalloc, rust.use_jemalloc);
            set(&mut config.backtrace, rust.backtrace);
            set(&mut config.channel, rust.channel.clone());
            set(&mut config.rust_dist_src, rust.dist_src);
            set(&mut config.verbose_tests, rust.verbose_tests);
            set(&mut config.test_miri, rust.test_miri);
            // in the case "false" is set explicitly, do not overwrite the command line args
            if let Some(true) = rust.incremental {
                config.incremental = true;
            }
            set(&mut config.wasm_syscall, rust.wasm_syscall);
            set(&mut config.lld_enabled, rust.lld);
            set(&mut config.lldb_enabled, rust.lldb);
            set(&mut config.llvm_tools_enabled, rust.llvm_tools);
            config.rustc_parallel_queries = rust.experimental_parallel_queries.unwrap_or(false);
            config.rustc_default_linker = rust.default_linker.clone();
            config.musl_root = rust.musl_root.clone().map(PathBuf::from);
            config.save_toolstates = rust.save_toolstates.clone().map(PathBuf::from);
            set(&mut config.deny_warnings, rust.deny_warnings.or(flags.warnings));
            set(&mut config.backtrace_on_ice, rust.backtrace_on_ice);
            set(&mut config.rust_verify_llvm_ir, rust.verify_llvm_ir);

            if let Some(ref backends) = rust.codegen_backends {
                config.rust_codegen_backends = backends.iter()
                    .map(|s| INTERNER.intern_str(s))
                    .collect();
            }

            set(&mut config.rust_codegen_backends_dir, rust.codegen_backends_dir.clone());

            match rust.codegen_units {
                Some(0) => config.rust_codegen_units = Some(num_cpus::get() as u32),
                Some(n) => config.rust_codegen_units = Some(n),
                None => {}
            }
        }

        if let Some(ref t) = toml.target {
            for (triple, cfg) in t {
                let mut target = Target::default();

                if let Some(ref s) = cfg.llvm_config {
                    target.llvm_config = Some(config.src.join(s));
                }
                if let Some(ref s) = cfg.jemalloc {
                    target.jemalloc = Some(config.src.join(s));
                }
                if let Some(ref s) = cfg.android_ndk {
                    target.ndk = Some(config.src.join(s));
                }
                target.cc = cfg.cc.clone().map(PathBuf::from);
                target.cxx = cfg.cxx.clone().map(PathBuf::from);
                target.ar = cfg.ar.clone().map(PathBuf::from);
                target.ranlib = cfg.ranlib.clone().map(PathBuf::from);
                target.linker = cfg.linker.clone().map(PathBuf::from);
                target.crt_static = cfg.crt_static.clone();
                target.musl_root = cfg.musl_root.clone().map(PathBuf::from);
                target.qemu_rootfs = cfg.qemu_rootfs.clone().map(PathBuf::from);

                config.target_config.insert(INTERNER.intern_string(triple.clone()), target);
            }
        }

        if let Some(ref t) = toml.dist {
            config.dist_sign_folder = t.sign_folder.clone().map(PathBuf::from);
            config.dist_gpg_password_file = t.gpg_password_file.clone().map(PathBuf::from);
            config.dist_upload_addr = t.upload_addr.clone();
            set(&mut config.rust_dist_src, t.src_tarball);
        }

        // Now that we've reached the end of our configuration, infer the
        // default values for all options that we haven't otherwise stored yet.

        set(&mut config.initial_rustc, build.rustc.map(PathBuf::from));
        set(&mut config.initial_cargo, build.cargo.map(PathBuf::from));

        let default = false;
        config.llvm_assertions = llvm_assertions.unwrap_or(default);

        let default = match &config.channel[..] {
            "stable" | "beta" | "nightly" => true,
            _ => false,
        };
        config.rust_debuginfo_lines = debuginfo_lines.unwrap_or(default);
        config.rust_debuginfo_only_std = debuginfo_only_std.unwrap_or(default);
        config.rust_debuginfo_tools = debuginfo_tools.unwrap_or(false);

        let default = debug == Some(true);
        config.debug_jemalloc = debug_jemalloc.unwrap_or(default);
        config.rust_debuginfo = debuginfo.unwrap_or(default);
        config.rust_debug_assertions = debug_assertions.unwrap_or(default);
        config.rust_optimize = optimize.unwrap_or(!default);

        let default = config.channel == "dev";
        config.ignore_git = ignore_git.unwrap_or(default);

        config
    }

    /// Try to find the relative path of `libdir`.
    pub fn libdir_relative(&self) -> Option<&Path> {
        let libdir = self.libdir.as_ref()?;
        if libdir.is_relative() {
            Some(libdir)
        } else {
            // Try to make it relative to the prefix.
            libdir.strip_prefix(self.prefix.as_ref()?).ok()
        }
    }

    pub fn verbose(&self) -> bool {
        self.verbose > 0
    }

    pub fn very_verbose(&self) -> bool {
        self.verbose > 1
    }
}

fn set<T>(field: &mut T, val: Option<T>) {
    if let Some(v) = val {
        *field = v;
    }
}
// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of the various distribution aspects of the compiler.
//
// This module is responsible for creating tarballs of the standard library,
// compiler, and documentation. This ends up being what we distribute to
// everyone as well.
//
// No tarball is actually created literally in this file, but rather we shell
// out to `rust-installer` still. This may one day be replaced with bits and
// pieces of `rustup.rs`!

use std::env;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{PathBuf, Path};
use std::process::{Command, Stdio};

use build_helper::output;

use {Compiler, Mode, LLVM_TOOLS};
use channel;
use util::{libdir, is_dylib, exe};
use builder::{Builder, RunConfig, ShouldRun, Step};
use compile;
use native;
use tool::{self, Tool};
use cache::{INTERNER, Interned};
use time;

pub fn pkgname(builder: &Builder, component: &str) -> String {
    if component == "cargo" {
        format!("{}-{}", component, builder.cargo_package_vers())
    } else if component == "rls" {
        format!("{}-{}", component, builder.rls_package_vers())
    } else if component == "clippy" {
        format!("{}-{}", component, builder.clippy_package_vers())
    } else if component == "rustfmt" {
        format!("{}-{}", component, builder.rustfmt_package_vers())
    } else if component == "llvm-tools" {
        format!("{}-{}", component, builder.llvm_tools_package_vers())
    } else if component == "lldb" {
        format!("{}-{}", component, builder.lldb_package_vers())
    } else {
        assert!(component.starts_with("rust"));
        format!("{}-{}", component, builder.rust_package_vers())
    }
}

fn distdir(builder: &Builder) -> PathBuf {
    builder.out.join("dist")
}

pub fn tmpdir(builder: &Builder) -> PathBuf {
    builder.out.join("tmp/dist")
}

fn rust_installer(builder: &Builder) -> Command {
    builder.tool_cmd(Tool::RustInstaller)
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Docs {
    pub stage: u32,
    pub host: Interned<String>,
}

impl Step for Docs {
    type Output = PathBuf;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/doc")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Docs {
            stage: run.builder.top_stage,
            host: run.target,
        });
    }

    /// Builds the `rust-docs` installer component.
    fn run(self, builder: &Builder) -> PathBuf {
        let host = self.host;

        let name = pkgname(builder, "rust-docs");

        builder.info(&format!("Dist docs ({})", host));
        if !builder.config.docs {
            builder.info("\tskipping - docs disabled");
            return distdir(builder).join(format!("{}-{}.tar.gz", name, host));
        }

        builder.default_doc(None);

        let image = tmpdir(builder).join(format!("{}-{}-image", name, host));
        let _ = fs::remove_dir_all(&image);

        let dst = image.join("share/doc/rust/html");
        t!(fs::create_dir_all(&dst));
        let src = builder.doc_out(host);
        builder.cp_r(&src, &dst);

        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust-Documentation")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Rust-documentation-is-installed.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}-{}", name, host))
           .arg("--component-name=rust-docs")
           .arg("--legacy-manifest-dirs=rustlib,cargo")
           .arg("--bulk-dirs=share/doc/rust/html");
        builder.run(&mut cmd);
        builder.remove_dir(&image);

        distdir(builder).join(format!("{}-{}.tar.gz", name, host))
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct RustcDocs {
    pub stage: u32,
    pub host: Interned<String>,
}

impl Step for RustcDocs {
    type Output = PathBuf;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/librustc")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(RustcDocs {
            stage: run.builder.top_stage,
            host: run.target,
        });
    }

    /// Builds the `rustc-docs` installer component.
    fn run(self, builder: &Builder) -> PathBuf {
        let host = self.host;

        let name = pkgname(builder, "rustc-docs");

        builder.info(&format!("Dist compiler docs ({})", host));
        if !builder.config.compiler_docs {
            builder.info("\tskipping - compiler docs disabled");
            return distdir(builder).join(format!("{}-{}.tar.gz", name, host));
        }

        builder.default_doc(None);

        let image = tmpdir(builder).join(format!("{}-{}-image", name, host));
        let _ = fs::remove_dir_all(&image);

        let dst = image.join("share/doc/rust/html");
        t!(fs::create_dir_all(&dst));
        let src = builder.compiler_doc_out(host);
        builder.cp_r(&src, &dst);

        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rustc-Documentation")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Rustc-documentation-is-installed.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}-{}", name, host))
           .arg("--component-name=rustc-docs")
           .arg("--legacy-manifest-dirs=rustlib,cargo")
           .arg("--bulk-dirs=share/doc/rust/html");
        builder.run(&mut cmd);
        builder.remove_dir(&image);

        distdir(builder).join(format!("{}-{}.tar.gz", name, host))
    }
}

fn find_files(files: &[&str], path: &[PathBuf]) -> Vec<PathBuf> {
    let mut found = Vec::with_capacity(files.len());

    for file in files {
        let file_path =
            path.iter()
                .map(|dir| dir.join(file))
                .find(|p| p.exists());

        if let Some(file_path) = file_path {
            found.push(file_path);
        } else {
            panic!("Could not find '{}' in {:?}", file, path);
        }
    }

    found
}

fn make_win_dist(
    rust_root: &Path, plat_root: &Path, target_triple: Interned<String>, builder: &Builder
) {
    //Ask gcc where it keeps its stuff
    let mut cmd = Command::new(builder.cc(target_triple));
    cmd.arg("-print-search-dirs");
    let gcc_out = output(&mut cmd);

    let mut bin_path: Vec<_> = env::split_paths(&env::var_os("PATH").unwrap_or_default()).collect();
    let mut lib_path = Vec::new();

    for line in gcc_out.lines() {
        let idx = line.find(':').unwrap();
        let key = &line[..idx];
        let trim_chars: &[_] = &[' ', '='];
        let value =
            line[(idx + 1)..]
                .trim_left_matches(trim_chars)
                .split(';')
                .map(PathBuf::from);

        if key == "programs" {
            bin_path.extend(value);
        } else if key == "libraries" {
            lib_path.extend(value);
        }
    }

    let target_tools = ["gcc.exe", "ld.exe", "dlltool.exe", "libwinpthread-1.dll"];
    let mut rustc_dlls = vec!["libstdc++-6.dll", "libwinpthread-1.dll"];
    if target_triple.starts_with("i686-") {
        rustc_dlls.push("libgcc_s_dw2-1.dll");
    } else {
        rustc_dlls.push("libgcc_s_seh-1.dll");
    }

    let target_libs = [ //MinGW libs
        "libgcc.a",
        "libgcc_eh.a",
        "libgcc_s.a",
        "libm.a",
        "libmingw32.a",
        "libmingwex.a",
        "libstdc++.a",
        "libiconv.a",
        "libmoldname.a",
        "libpthread.a",
        //Windows import libs
        "libadvapi32.a",
        "libbcrypt.a",
        "libcomctl32.a",
        "libcomdlg32.a",
        "libcredui.a",
        "libcrypt32.a",
        "libdbghelp.a",
        "libgdi32.a",
        "libimagehlp.a",
        "libiphlpapi.a",
        "libkernel32.a",
        "libmsimg32.a",
        "libmsvcrt.a",
        "libodbc32.a",
        "libole32.a",
        "liboleaut32.a",
        "libopengl32.a",
        "libpsapi.a",
        "librpcrt4.a",
        "libsecur32.a",
        "libsetupapi.a",
        "libshell32.a",
        "libsynchronization.a",
        "libuser32.a",
        "libuserenv.a",
        "libuuid.a",
        "libwinhttp.a",
        "libwinmm.a",
        "libwinspool.a",
        "libws2_32.a",
        "libwsock32.a",
    ];

    //Find mingw artifacts we want to bundle
    let target_tools = find_files(&target_tools, &bin_path);
    let rustc_dlls = find_files(&rustc_dlls, &bin_path);
    let target_libs = find_files(&target_libs, &lib_path);

    // Copy runtime dlls next to rustc.exe
    let dist_bin_dir = rust_root.join("bin/");
    fs::create_dir_all(&dist_bin_dir).expect("creating dist_bin_dir failed");
    for src in rustc_dlls {
        builder.copy_to_folder(&src, &dist_bin_dir);
    }

    //Copy platform tools to platform-specific bin directory
    let target_bin_dir = plat_root.join("lib").join("rustlib").join(target_triple).join("bin");
    fs::create_dir_all(&target_bin_dir).expect("creating target_bin_dir failed");
    for src in target_tools {
        builder.copy_to_folder(&src, &target_bin_dir);
    }

    // Warn windows-gnu users that the bundled GCC cannot compile C files
    builder.create(
        &target_bin_dir.join("GCC-WARNING.txt"),
        "gcc.exe contained in this folder cannot be used for compiling C files - it is only\
         used as a linker. In order to be able to compile projects containing C code use\
         the GCC provided by MinGW or Cygwin."
    );

    //Copy platform libs to platform-specific lib directory
    let target_lib_dir = plat_root.join("lib").join("rustlib").join(target_triple).join("lib");
    fs::create_dir_all(&target_lib_dir).expect("creating target_lib_dir failed");
    for src in target_libs {
        builder.copy_to_folder(&src, &target_lib_dir);
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Mingw {
    pub host: Interned<String>,
}

impl Step for Mingw {
    type Output = Option<PathBuf>;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Mingw { host: run.target });
    }

    /// Build the `rust-mingw` installer component.
    ///
    /// This contains all the bits and pieces to run the MinGW Windows targets
    /// without any extra installed software (e.g. we bundle gcc, libraries, etc).
    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let host = self.host;

        if !host.contains("pc-windows-gnu") {
            return None;
        }

        builder.info(&format!("Dist mingw ({})", host));
        let name = pkgname(builder, "rust-mingw");
        let image = tmpdir(builder).join(format!("{}-{}-image", name, host));
        let _ = fs::remove_dir_all(&image);
        t!(fs::create_dir_all(&image));

        // The first argument is a "temporary directory" which is just
        // thrown away (this contains the runtime DLLs included in the rustc package
        // above) and the second argument is where to place all the MinGW components
        // (which is what we want).
        make_win_dist(&tmpdir(builder), &image, host, &builder);

        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust-MinGW")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Rust-MinGW-is-installed.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}-{}", name, host))
           .arg("--component-name=rust-mingw")
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);
        t!(fs::remove_dir_all(&image));
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, host)))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Rustc {
    pub compiler: Compiler,
}

impl Step for Rustc {
    type Output = PathBuf;
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/librustc")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustc {
            compiler: run.builder.compiler(run.builder.top_stage, run.target),
        });
    }

    /// Creates the `rustc` installer component.
    fn run(self, builder: &Builder) -> PathBuf {
        let compiler = self.compiler;
        let host = self.compiler.host;

        builder.info(&format!("Dist rustc stage{} ({})", compiler.stage, host));
        let name = pkgname(builder, "rustc");
        let image = tmpdir(builder).join(format!("{}-{}-image", name, host));
        let _ = fs::remove_dir_all(&image);
        let overlay = tmpdir(builder).join(format!("{}-{}-overlay", name, host));
        let _ = fs::remove_dir_all(&overlay);

        // Prepare the rustc "image", what will actually end up getting installed
        prepare_image(builder, compiler, &image);

        // Prepare the overlay which is part of the tarball but won't actually be
        // installed
        let cp = |file: &str| {
            builder.install(&builder.src.join(file), &overlay, 0o644);
        };
        cp("COPYRIGHT");
        cp("LICENSE-APACHE");
        cp("LICENSE-MIT");
        cp("README.md");
        // tiny morsel of metadata is used by rust-packaging
        let version = builder.rust_version();
        builder.create(&overlay.join("version"), &version);
        if let Some(sha) = builder.rust_sha() {
            builder.create(&overlay.join("git-commit-hash"), &sha);
        }

        // On MinGW we've got a few runtime DLL dependencies that we need to
        // include. The first argument to this script is where to put these DLLs
        // (the image we're creating), and the second argument is a junk directory
        // to ignore all other MinGW stuff the script creates.
        //
        // On 32-bit MinGW we're always including a DLL which needs some extra
        // licenses to distribute. On 64-bit MinGW we don't actually distribute
        // anything requiring us to distribute a license, but it's likely the
        // install will *also* include the rust-mingw package, which also needs
        // licenses, so to be safe we just include it here in all MinGW packages.
        if host.contains("pc-windows-gnu") {
            make_win_dist(&image, &tmpdir(builder), host, builder);

            let dst = image.join("share/doc");
            t!(fs::create_dir_all(&dst));
            builder.cp_r(&builder.src.join("src/etc/third-party"), &dst);
        }

        // Finally, wrap everything up in a nice tarball!
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Rust-is-ready-to-roll.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg("--non-installed-overlay").arg(&overlay)
           .arg(format!("--package-name={}-{}", name, host))
           .arg("--component-name=rustc")
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);
        builder.remove_dir(&image);
        builder.remove_dir(&overlay);

        return distdir(builder).join(format!("{}-{}.tar.gz", name, host));

        fn prepare_image(builder: &Builder, compiler: Compiler, image: &Path) {
            let host = compiler.host;
            let src = builder.sysroot(compiler);
            let libdir = libdir(&host);

            // Copy rustc/rustdoc binaries
            t!(fs::create_dir_all(image.join("bin")));
            builder.cp_r(&src.join("bin"), &image.join("bin"));

            builder.install(&builder.rustdoc(compiler.host), &image.join("bin"), 0o755);

            // Copy runtime DLLs needed by the compiler
            if libdir != "bin" {
                for entry in builder.read_dir(&src.join(libdir)) {
                    let name = entry.file_name();
                    if let Some(s) = name.to_str() {
                        if is_dylib(s) {
                            builder.install(&entry.path(), &image.join(libdir), 0o644);
                        }
                    }
                }
            }

            // Copy over the codegen backends
            let backends_src = builder.sysroot_codegen_backends(compiler);
            let backends_rel = backends_src.strip_prefix(&src).unwrap();
            let backends_dst = image.join(&backends_rel);
            t!(fs::create_dir_all(&backends_dst));
            builder.cp_r(&backends_src, &backends_dst);

            // Copy libLLVM.so to the lib dir as well, if needed. While not
            // technically needed by rustc itself it's needed by lots of other
            // components like the llvm tools and LLD. LLD is included below and
            // tools/LLDB come later, so let's just throw it in the rustc
            // component for now.
            maybe_install_llvm_dylib(builder, host, image);

            // Copy over lld if it's there
            if builder.config.lld_enabled {
                let exe = exe("rust-lld", &compiler.host);
                let src = builder.sysroot_libdir(compiler, host)
                    .parent()
                    .unwrap()
                    .join("bin")
                    .join(&exe);
                // for the rationale about this rename check `compile::copy_lld_to_sysroot`
                let dst = image.join("lib/rustlib")
                    .join(&*host)
                    .join("bin")
                    .join(&exe);
                t!(fs::create_dir_all(&dst.parent().unwrap()));
                builder.copy(&src, &dst);
            }

            // Man pages
            t!(fs::create_dir_all(image.join("share/man/man1")));
            let man_src = builder.src.join("src/doc/man");
            let man_dst = image.join("share/man/man1");
            let month_year = t!(time::strftime("%B %Y", &time::now()));
            // don't use our `bootstrap::util::{copy, cp_r}`, because those try
            // to hardlink, and we don't want to edit the source templates
            for file_entry in builder.read_dir(&man_src) {
                let page_src = file_entry.path();
                let page_dst = man_dst.join(file_entry.file_name());
                t!(fs::copy(&page_src, &page_dst));
                // template in month/year and version number
                builder.replace_in_file(&page_dst,
                                &[("<INSERT DATE HERE>", &month_year),
                                  ("<INSERT VERSION HERE>", channel::CFG_RELEASE_NUM)]);
            }

            // Debugger scripts
            builder.ensure(DebuggerScripts {
                sysroot: INTERNER.intern_path(image.to_owned()),
                host,
            });

            // Misc license info
            let cp = |file: &str| {
                builder.install(&builder.src.join(file), &image.join("share/doc/rust"), 0o644);
            };
            cp("COPYRIGHT");
            cp("LICENSE-APACHE");
            cp("LICENSE-MIT");
            cp("README.md");
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct DebuggerScripts {
    pub sysroot: Interned<PathBuf>,
    pub host: Interned<String>,
}

impl Step for DebuggerScripts {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/lldb_batchmode.py")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(DebuggerScripts {
            sysroot: run.builder.sysroot(run.builder.compiler(run.builder.top_stage, run.host)),
            host: run.target,
        });
    }

    /// Copies debugger scripts for `target` into the `sysroot` specified.
    fn run(self, builder: &Builder) {
        let host = self.host;
        let sysroot = self.sysroot;
        let dst = sysroot.join("lib/rustlib/etc");
        t!(fs::create_dir_all(&dst));
        let cp_debugger_script = |file: &str| {
            builder.install(&builder.src.join("src/etc/").join(file), &dst, 0o644);
        };
        if host.contains("windows-msvc") {
            // windbg debugger scripts
            builder.install(&builder.src.join("src/etc/rust-windbg.cmd"), &sysroot.join("bin"),
                0o755);

            cp_debugger_script("natvis/intrinsic.natvis");
            cp_debugger_script("natvis/liballoc.natvis");
            cp_debugger_script("natvis/libcore.natvis");
        } else {
            cp_debugger_script("debugger_pretty_printers_common.py");

            // gdb debugger scripts
            builder.install(&builder.src.join("src/etc/rust-gdb"), &sysroot.join("bin"),
                    0o755);

            cp_debugger_script("gdb_load_rust_pretty_printers.py");
            cp_debugger_script("gdb_rust_pretty_printing.py");

            // lldb debugger scripts
            builder.install(&builder.src.join("src/etc/rust-lldb"), &sysroot.join("bin"),
                    0o755);

            cp_debugger_script("lldb_rust_formatters.py");
        }
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Std {
    pub compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for Std {
    type Output = PathBuf;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/libstd")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Std {
            compiler: run.builder.compiler(run.builder.top_stage, run.builder.config.build),
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> PathBuf {
        let compiler = self.compiler;
        let target = self.target;

        let name = pkgname(builder, "rust-std");
        builder.info(&format!("Dist std stage{} ({} -> {})",
            compiler.stage, &compiler.host, target));

        // The only true set of target libraries came from the build triple, so
        // let's reduce redundant work by only producing archives from that host.
        if compiler.host != builder.config.build {
            builder.info("\tskipping, not a build host");
            return distdir(builder).join(format!("{}-{}.tar.gz", name, target));
        }

        // We want to package up as many target libraries as possible
        // for the `rust-std` package, so if this is a host target we
        // depend on librustc and otherwise we just depend on libtest.
        if builder.hosts.iter().any(|t| t == target) {
            builder.ensure(compile::Rustc { compiler, target });
        } else {
            if builder.no_std(target) == Some(true) {
                // the `test` doesn't compile for no-std targets
                builder.ensure(compile::Std { compiler, target });
            } else {
                builder.ensure(compile::Test { compiler, target });
            }
        }

        let image = tmpdir(builder).join(format!("{}-{}-image", name, target));
        let _ = fs::remove_dir_all(&image);

        let dst = image.join("lib/rustlib").join(target);
        t!(fs::create_dir_all(&dst));
        let mut src = builder.sysroot_libdir(compiler, target).to_path_buf();
        src.pop(); // Remove the trailing /lib folder from the sysroot_libdir
        builder.cp_filtered(&src, &dst, &|path| {
            let name = path.file_name().and_then(|s| s.to_str());
            name != Some(builder.config.rust_codegen_backends_dir.as_str()) &&
                name != Some("bin")

        });

        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=std-is-standing-at-the-ready.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}-{}", name, target))
           .arg(format!("--component-name=rust-std-{}", target))
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);
        builder.remove_dir(&image);
        distdir(builder).join(format!("{}-{}.tar.gz", name, target))
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Analysis {
    pub compiler: Compiler,
    pub target: Interned<String>,
}

impl Step for Analysis {
    type Output = PathBuf;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("analysis").default_condition(builder.config.extended)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Analysis {
            compiler: run.builder.compiler(run.builder.top_stage, run.builder.config.build),
            target: run.target,
        });
    }

    /// Creates a tarball of save-analysis metadata, if available.
    fn run(self, builder: &Builder) -> PathBuf {
        let compiler = self.compiler;
        let target = self.target;
        assert!(builder.config.extended);
        builder.info("Dist analysis");
        let name = pkgname(builder, "rust-analysis");

        if &compiler.host != builder.config.build {
            builder.info("\tskipping, not a build host");
            return distdir(builder).join(format!("{}-{}.tar.gz", name, target));
        }

        builder.ensure(Std { compiler, target });

        // Package save-analysis from stage1 if not doing a full bootstrap, as the
        // stage2 artifacts is simply copied from stage1 in that case.
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler.clone()
        };

        let image = tmpdir(builder).join(format!("{}-{}-image", name, target));

        let src = builder.stage_out(compiler, Mode::Std)
            .join(target).join(builder.cargo_dir()).join("deps");

        let image_src = src.join("save-analysis");
        let dst = image.join("lib/rustlib").join(target).join("analysis");
        t!(fs::create_dir_all(&dst));
        builder.info(&format!("image_src: {:?}, dst: {:?}", image_src, dst));
        builder.cp_r(&image_src, &dst);

        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=save-analysis-saved.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}-{}", name, target))
           .arg(format!("--component-name=rust-analysis-{}", target))
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);
        builder.remove_dir(&image);
        distdir(builder).join(format!("{}-{}.tar.gz", name, target))
    }
}

fn copy_src_dirs(builder: &Builder, src_dirs: &[&str], exclude_dirs: &[&str], dst_dir: &Path) {
    fn filter_fn(exclude_dirs: &[&str], dir: &str, path: &Path) -> bool {
        let spath = match path.to_str() {
            Some(path) => path,
            None => return false,
        };
        if spath.ends_with("~") || spath.ends_with(".pyc") {
            return false
        }
        if (spath.contains("llvm/test") || spath.contains("llvm\\test")) &&
            (spath.ends_with(".ll") ||
             spath.ends_with(".td") ||
             spath.ends_with(".s")) {
            return false
        }
        if spath.contains("test/emscripten") || spath.contains("test\\emscripten") {
            return false
        }

        let full_path = Path::new(dir).join(path);
        if exclude_dirs.iter().any(|excl| full_path == Path::new(excl)) {
            return false;
        }

        let excludes = [
            "CVS", "RCS", "SCCS", ".git", ".gitignore", ".gitmodules",
            ".gitattributes", ".cvsignore", ".svn", ".arch-ids", "{arch}",
            "=RELEASE-ID", "=meta-update", "=update", ".bzr", ".bzrignore",
            ".bzrtags", ".hg", ".hgignore", ".hgrags", "_darcs",
        ];
        !path.iter()
             .map(|s| s.to_str().unwrap())
             .any(|s| excludes.contains(&s))
    }

    // Copy the directories using our filter
    for item in src_dirs {
        let dst = &dst_dir.join(item);
        t!(fs::create_dir_all(dst));
        builder.cp_filtered(
            &builder.src.join(item), dst, &|path| filter_fn(exclude_dirs, item, path));
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Src;

impl Step for Src {
    /// The output path of the src installer tarball
    type Output = PathBuf;
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Src);
    }

    /// Creates the `rust-src` installer component
    fn run(self, builder: &Builder) -> PathBuf {
        builder.info("Dist src");

        let name = pkgname(builder, "rust-src");
        let image = tmpdir(builder).join(format!("{}-image", name));
        let _ = fs::remove_dir_all(&image);

        let dst = image.join("lib/rustlib/src");
        let dst_src = dst.join("rust");
        t!(fs::create_dir_all(&dst_src));

        let src_files = [
            "src/Cargo.lock",
        ];
        // This is the reduced set of paths which will become the rust-src component
        // (essentially libstd and all of its path dependencies)
        let std_src_dirs = [
            "src/build_helper",
            "src/dlmalloc",
            "src/liballoc",
            "src/liballoc_jemalloc",
            "src/liballoc_system",
            "src/libbacktrace",
            "src/libcompiler_builtins",
            "src/libcore",
            "src/liblibc",
            "src/libpanic_abort",
            "src/libpanic_unwind",
            "src/librustc_asan",
            "src/librustc_lsan",
            "src/librustc_msan",
            "src/librustc_tsan",
            "src/libstd",
            "src/libunwind",
            "src/rustc/compiler_builtins_shim",
            "src/rustc/libc_shim",
            "src/rustc/dlmalloc_shim",
            "src/libtest",
            "src/libterm",
            "src/jemalloc",
            "src/libprofiler_builtins",
            "src/stdsimd",
        ];
        let std_src_dirs_exclude = [
            "src/libcompiler_builtins/compiler-rt/test",
            "src/jemalloc/test/unit",
        ];

        copy_src_dirs(builder, &std_src_dirs[..], &std_src_dirs_exclude[..], &dst_src);
        for file in src_files.iter() {
            builder.copy(&builder.src.join(file), &dst_src.join(file));
        }

        // Create source tarball in rust-installer format
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Awesome-Source.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg(format!("--package-name={}", name))
           .arg("--component-name=rust-src")
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);

        builder.remove_dir(&image);
        distdir(builder).join(&format!("{}.tar.gz", name))
    }
}

const CARGO_VENDOR_VERSION: &str = "0.1.4";

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct PlainSourceTarball;

impl Step for PlainSourceTarball {
    /// Produces the location of the tarball generated
    type Output = PathBuf;
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src").default_condition(builder.config.rust_dist_src)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(PlainSourceTarball);
    }

    /// Creates the plain source tarball
    fn run(self, builder: &Builder) -> PathBuf {
        builder.info("Create plain source tarball");

        // Make sure that the root folder of tarball has the correct name
        let plain_name = format!("{}-src", pkgname(builder, "rustc"));
        let plain_dst_src = tmpdir(builder).join(&plain_name);
        let _ = fs::remove_dir_all(&plain_dst_src);
        t!(fs::create_dir_all(&plain_dst_src));

        // This is the set of root paths which will become part of the source package
        let src_files = [
            "COPYRIGHT",
            "LICENSE-APACHE",
            "LICENSE-MIT",
            "CONTRIBUTING.md",
            "README.md",
            "RELEASES.md",
            "configure",
            "x.py",
            "config.toml.example",
        ];
        let src_dirs = [
            "src",
        ];

        copy_src_dirs(builder, &src_dirs[..], &[], &plain_dst_src);

        // Copy the files normally
        for item in &src_files {
            builder.copy(&builder.src.join(item), &plain_dst_src.join(item));
        }

        // Create the version file
        builder.create(&plain_dst_src.join("version"), &builder.rust_version());
        if let Some(sha) = builder.rust_sha() {
            builder.create(&plain_dst_src.join("git-commit-hash"), &sha);
        }

        // If we're building from git sources, we need to vendor a complete distribution.
        if builder.rust_info.is_git() {
            // Get cargo-vendor installed, if it isn't already.
            let mut has_cargo_vendor = false;
            let mut cmd = Command::new(&builder.initial_cargo);
            for line in output(cmd.arg("install").arg("--list")).lines() {
                has_cargo_vendor |= line.starts_with("cargo-vendor ");
            }
            if !has_cargo_vendor {
                let mut cmd = builder.cargo(
                    builder.compiler(0, builder.config.build),
                    Mode::ToolBootstrap,
                    builder.config.build,
                    "install"
                );
                cmd.arg("--force")
                   .arg("--debug")
                   .arg("--vers").arg(CARGO_VENDOR_VERSION)
                   .arg("cargo-vendor");
                if let Some(dir) = builder.openssl_install_dir(builder.config.build) {
                    builder.ensure(native::Openssl {
                        target: builder.config.build,
                    });
                    cmd.env("OPENSSL_DIR", dir);
                }
                builder.run(&mut cmd);
            }

            // Vendor all Cargo dependencies
            let mut cmd = Command::new(&builder.initial_cargo);
            cmd.arg("vendor")
               .current_dir(&plain_dst_src.join("src"));
            builder.run(&mut cmd);
        }

        // Create plain source tarball
        let plain_name = format!("rustc-{}-src", builder.rust_package_vers());
        let mut tarball = distdir(builder).join(&format!("{}.tar.gz", plain_name));
        tarball.set_extension(""); // strip .gz
        tarball.set_extension(""); // strip .tar
        if let Some(dir) = tarball.parent() {
            builder.create_dir(&dir);
        }
        builder.info("running installer");
        let mut cmd = rust_installer(builder);
        cmd.arg("tarball")
           .arg("--input").arg(&plain_name)
           .arg("--output").arg(&tarball)
           .arg("--work-dir=.")
           .current_dir(tmpdir(builder));
        builder.run(&mut cmd);
        distdir(builder).join(&format!("{}.tar.gz", plain_name))
    }
}

// We have to run a few shell scripts, which choke quite a bit on both `\`
// characters and on `C:\` paths, so normalize both of them away.
pub fn sanitize_sh(path: &Path) -> String {
    let path = path.to_str().unwrap().replace("\\", "/");
    return change_drive(&path).unwrap_or(path);

    fn change_drive(s: &str) -> Option<String> {
        let mut ch = s.chars();
        let drive = ch.next().unwrap_or('C');
        if ch.next() != Some(':') {
            return None
        }
        if ch.next() != Some('/') {
            return None
        }
        Some(format!("/{}/{}", drive, &s[drive.len_utf8() + 2..]))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Cargo {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for Cargo {
    type Output = PathBuf;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("cargo")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Cargo {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> PathBuf {
        let stage = self.stage;
        let target = self.target;

        builder.info(&format!("Dist cargo stage{} ({})", stage, target));
        let src = builder.src.join("src/tools/cargo");
        let etc = src.join("src/etc");
        let release_num = builder.release_num("cargo");
        let name = pkgname(builder, "cargo");
        let version = builder.cargo_info.version(builder, &release_num);

        let tmp = tmpdir(builder);
        let image = tmp.join("cargo-image");
        drop(fs::remove_dir_all(&image));
        builder.create_dir(&image);

        // Prepare the image directory
        builder.create_dir(&image.join("share/zsh/site-functions"));
        builder.create_dir(&image.join("etc/bash_completion.d"));
        let cargo = builder.ensure(tool::Cargo {
            compiler: builder.compiler(stage, builder.config.build),
            target
        });
        builder.install(&cargo, &image.join("bin"), 0o755);
        for man in t!(etc.join("man").read_dir()) {
            let man = t!(man);
            builder.install(&man.path(), &image.join("share/man/man1"), 0o644);
        }
        builder.install(&etc.join("_cargo"), &image.join("share/zsh/site-functions"), 0o644);
        builder.copy(&etc.join("cargo.bashcomp.sh"),
             &image.join("etc/bash_completion.d/cargo"));
        let doc = image.join("share/doc/cargo");
        builder.install(&src.join("README.md"), &doc, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &doc, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &doc, 0o644);
        builder.install(&src.join("LICENSE-THIRD-PARTY"), &doc, 0o644);

        // Prepare the overlay
        let overlay = tmp.join("cargo-overlay");
        drop(fs::remove_dir_all(&overlay));
        builder.create_dir(&overlay);
        builder.install(&src.join("README.md"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-THIRD-PARTY"), &overlay, 0o644);
        builder.create(&overlay.join("version"), &version);

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=Rust-is-ready-to-roll.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg("--non-installed-overlay").arg(&overlay)
           .arg(format!("--package-name={}-{}", name, target))
           .arg("--component-name=cargo")
           .arg("--legacy-manifest-dirs=rustlib,cargo");
        builder.run(&mut cmd);
        distdir(builder).join(format!("{}-{}.tar.gz", name, target))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Rls {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for Rls {
    type Output = Option<PathBuf>;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("rls")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rls {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let stage = self.stage;
        let target = self.target;
        assert!(builder.config.extended);

        builder.info(&format!("Dist RLS stage{} ({})", stage, target));
        let src = builder.src.join("src/tools/rls");
        let release_num = builder.release_num("rls");
        let name = pkgname(builder, "rls");
        let version = builder.rls_info.version(builder, &release_num);

        let tmp = tmpdir(builder);
        let image = tmp.join("rls-image");
        drop(fs::remove_dir_all(&image));
        t!(fs::create_dir_all(&image));

        // Prepare the image directory
        // We expect RLS to build, because we've exited this step above if tool
        // state for RLS isn't testing.
        let rls = builder.ensure(tool::Rls {
            compiler: builder.compiler(stage, builder.config.build),
            target, extra_features: Vec::new()
        }).or_else(|| { println!("Unable to build RLS, skipping dist"); None })?;

        builder.install(&rls, &image.join("bin"), 0o755);
        let doc = image.join("share/doc/rls");
        builder.install(&src.join("README.md"), &doc, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &doc, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &doc, 0o644);

        // Prepare the overlay
        let overlay = tmp.join("rls-overlay");
        drop(fs::remove_dir_all(&overlay));
        t!(fs::create_dir_all(&overlay));
        builder.install(&src.join("README.md"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &overlay, 0o644);
        builder.create(&overlay.join("version"), &version);

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=RLS-ready-to-serve.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg("--non-installed-overlay").arg(&overlay)
           .arg(format!("--package-name={}-{}", name, target))
           .arg("--legacy-manifest-dirs=rustlib,cargo")
           .arg("--component-name=rls-preview");

        builder.run(&mut cmd);
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, target)))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Clippy {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for Clippy {
    type Output = Option<PathBuf>;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("clippy")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Clippy {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let stage = self.stage;
        let target = self.target;
        assert!(builder.config.extended);

        builder.info(&format!("Dist clippy stage{} ({})", stage, target));
        let src = builder.src.join("src/tools/clippy");
        let release_num = builder.release_num("clippy");
        let name = pkgname(builder, "clippy");
        let version = builder.clippy_info.version(builder, &release_num);

        let tmp = tmpdir(builder);
        let image = tmp.join("clippy-image");
        drop(fs::remove_dir_all(&image));
        builder.create_dir(&image);

        // Prepare the image directory
        // We expect clippy to build, because we've exited this step above if tool
        // state for clippy isn't testing.
        let clippy = builder.ensure(tool::Clippy {
            compiler: builder.compiler(stage, builder.config.build),
            target, extra_features: Vec::new()
        }).or_else(|| { println!("Unable to build clippy, skipping dist"); None })?;
        let cargoclippy = builder.ensure(tool::CargoClippy {
            compiler: builder.compiler(stage, builder.config.build),
            target, extra_features: Vec::new()
        }).or_else(|| { println!("Unable to build cargo clippy, skipping dist"); None })?;

        builder.install(&clippy, &image.join("bin"), 0o755);
        builder.install(&cargoclippy, &image.join("bin"), 0o755);
        let doc = image.join("share/doc/clippy");
        builder.install(&src.join("README.md"), &doc, 0o644);
        builder.install(&src.join("LICENSE"), &doc, 0o644);

        // Prepare the overlay
        let overlay = tmp.join("clippy-overlay");
        drop(fs::remove_dir_all(&overlay));
        t!(fs::create_dir_all(&overlay));
        builder.install(&src.join("README.md"), &overlay, 0o644);
        builder.install(&src.join("LICENSE"), &doc, 0o644);
        builder.create(&overlay.join("version"), &version);

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=clippy-ready-to-serve.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg("--non-installed-overlay").arg(&overlay)
           .arg(format!("--package-name={}-{}", name, target))
           .arg("--legacy-manifest-dirs=rustlib,cargo")
           .arg("--component-name=clippy-preview");

        builder.run(&mut cmd);
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, target)))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Rustfmt {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for Rustfmt {
    type Output = Option<PathBuf>;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("rustfmt")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustfmt {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let stage = self.stage;
        let target = self.target;

        builder.info(&format!("Dist Rustfmt stage{} ({})", stage, target));
        let src = builder.src.join("src/tools/rustfmt");
        let release_num = builder.release_num("rustfmt");
        let name = pkgname(builder, "rustfmt");
        let version = builder.rustfmt_info.version(builder, &release_num);

        let tmp = tmpdir(builder);
        let image = tmp.join("rustfmt-image");
        drop(fs::remove_dir_all(&image));
        builder.create_dir(&image);

        // Prepare the image directory
        let rustfmt = builder.ensure(tool::Rustfmt {
            compiler: builder.compiler(stage, builder.config.build),
            target, extra_features: Vec::new()
        }).or_else(|| { println!("Unable to build Rustfmt, skipping dist"); None })?;
        let cargofmt = builder.ensure(tool::Cargofmt {
            compiler: builder.compiler(stage, builder.config.build),
            target, extra_features: Vec::new()
        }).or_else(|| { println!("Unable to build Cargofmt, skipping dist"); None })?;

        builder.install(&rustfmt, &image.join("bin"), 0o755);
        builder.install(&cargofmt, &image.join("bin"), 0o755);
        let doc = image.join("share/doc/rustfmt");
        builder.install(&src.join("README.md"), &doc, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &doc, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &doc, 0o644);

        // Prepare the overlay
        let overlay = tmp.join("rustfmt-overlay");
        drop(fs::remove_dir_all(&overlay));
        builder.create_dir(&overlay);
        builder.install(&src.join("README.md"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-MIT"), &overlay, 0o644);
        builder.install(&src.join("LICENSE-APACHE"), &overlay, 0o644);
        builder.create(&overlay.join("version"), &version);

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
           .arg("--product-name=Rust")
           .arg("--rel-manifest-dir=rustlib")
           .arg("--success-message=rustfmt-ready-to-fmt.")
           .arg("--image-dir").arg(&image)
           .arg("--work-dir").arg(&tmpdir(builder))
           .arg("--output-dir").arg(&distdir(builder))
           .arg("--non-installed-overlay").arg(&overlay)
           .arg(format!("--package-name={}-{}", name, target))
           .arg("--legacy-manifest-dirs=rustlib,cargo")
           .arg("--component-name=rustfmt-preview");

        builder.run(&mut cmd);
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, target)))
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Extended {
    stage: u32,
    host: Interned<String>,
    target: Interned<String>,
}

impl Step for Extended {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("extended").default_condition(builder.config.extended)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Extended {
            stage: run.builder.top_stage,
            host: run.builder.config.build,
            target: run.target,
        });
    }

    /// Creates a combined installer for the specified target in the provided stage.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;

        builder.info(&format!("Dist extended stage{} ({})", stage, target));

        let rustc_installer = builder.ensure(Rustc {
            compiler: builder.compiler(stage, target),
        });
        let cargo_installer = builder.ensure(Cargo { stage, target });
        let rustfmt_installer = builder.ensure(Rustfmt { stage, target });
        let rls_installer = builder.ensure(Rls { stage, target });
        let llvm_tools_installer = builder.ensure(LlvmTools { stage, target });
        let clippy_installer = builder.ensure(Clippy { stage, target });
        let lldb_installer = builder.ensure(Lldb { target });
        let mingw_installer = builder.ensure(Mingw { host: target });
        let analysis_installer = builder.ensure(Analysis {
            compiler: builder.compiler(stage, self.host),
            target
        });

        let docs_installer = builder.ensure(Docs { stage, host: target, });
        let std_installer = builder.ensure(Std {
            compiler: builder.compiler(stage, self.host),
            target,
        });

        let tmp = tmpdir(builder);
        let overlay = tmp.join("extended-overlay");
        let etc = builder.src.join("src/etc/installer");
        let work = tmp.join("work");

        let _ = fs::remove_dir_all(&overlay);
        builder.install(&builder.src.join("COPYRIGHT"), &overlay, 0o644);
        builder.install(&builder.src.join("LICENSE-APACHE"), &overlay, 0o644);
        builder.install(&builder.src.join("LICENSE-MIT"), &overlay, 0o644);
        let version = builder.rust_version();
        builder.create(&overlay.join("version"), &version);
        if let Some(sha) = builder.rust_sha() {
            builder.create(&overlay.join("git-commit-hash"), &sha);
        }
        builder.install(&etc.join("README.md"), &overlay, 0o644);

        // When rust-std package split from rustc, we needed to ensure that during
        // upgrades rustc was upgraded before rust-std. To avoid rustc clobbering
        // the std files during uninstall. To do this ensure that rustc comes
        // before rust-std in the list below.
        let mut tarballs = Vec::new();
        tarballs.push(rustc_installer);
        tarballs.push(cargo_installer);
        tarballs.extend(rls_installer.clone());
        tarballs.extend(clippy_installer.clone());
        tarballs.extend(rustfmt_installer.clone());
        tarballs.extend(llvm_tools_installer.clone());
        tarballs.extend(lldb_installer.clone());
        tarballs.push(analysis_installer);
        tarballs.push(std_installer);
        if builder.config.docs {
            tarballs.push(docs_installer);
        }
        if target.contains("pc-windows-gnu") {
            tarballs.push(mingw_installer.unwrap());
        }
        let mut input_tarballs = tarballs[0].as_os_str().to_owned();
        for tarball in &tarballs[1..] {
            input_tarballs.push(",");
            input_tarballs.push(tarball);
        }

        let mut cmd = rust_installer(builder);
        cmd.arg("combine")
            .arg("--product-name=Rust")
            .arg("--rel-manifest-dir=rustlib")
            .arg("--success-message=Rust-is-ready-to-roll.")
            .arg("--work-dir").arg(&work)
            .arg("--output-dir").arg(&distdir(builder))
            .arg(format!("--package-name={}-{}", pkgname(builder, "rust"), target))
            .arg("--legacy-manifest-dirs=rustlib,cargo")
            .arg("--input-tarballs").arg(input_tarballs)
            .arg("--non-installed-overlay").arg(&overlay);
        builder.run(&mut cmd);

        let mut license = String::new();
        license += &builder.read(&builder.src.join("COPYRIGHT"));
        license += &builder.read(&builder.src.join("LICENSE-APACHE"));
        license += &builder.read(&builder.src.join("LICENSE-MIT"));
        license.push_str("\n");
        license.push_str("\n");

        let rtf = r"{\rtf1\ansi\deff0{\fonttbl{\f0\fnil\fcharset0 Arial;}}\nowwrap\fs18";
        let mut rtf = rtf.to_string();
        rtf.push_str("\n");
        for line in license.lines() {
            rtf.push_str(line);
            rtf.push_str("\\line ");
        }
        rtf.push_str("}");

        fn filter(contents: &str, marker: &str) -> String {
            let start = format!("tool-{}-start", marker);
            let end = format!("tool-{}-end", marker);
            let mut lines = Vec::new();
            let mut omitted = false;
            for line in contents.lines() {
                if line.contains(&start) {
                    omitted = true;
                } else if line.contains(&end) {
                    omitted = false;
                } else if !omitted {
                    lines.push(line);
                }
            }

            lines.join("\n")
        }

        let xform = |p: &Path| {
            let mut contents = String::new();
            t!(t!(File::open(p)).read_to_string(&mut contents));
            if rls_installer.is_none() {
                contents = filter(&contents, "rls");
            }
            if clippy_installer.is_none() {
                contents = filter(&contents, "clippy");
            }
            if rustfmt_installer.is_none() {
                contents = filter(&contents, "rustfmt");
            }
            let ret = tmp.join(p.file_name().unwrap());
            t!(t!(File::create(&ret)).write_all(contents.as_bytes()));
            return ret
        };

        if target.contains("apple-darwin") {
            let pkg = tmp.join("pkg");
            let _ = fs::remove_dir_all(&pkg);

            let pkgbuild = |component: &str| {
                let mut cmd = Command::new("pkgbuild");
                cmd.arg("--identifier").arg(format!("org.rust-lang.{}", component))
                    .arg("--scripts").arg(pkg.join(component))
                    .arg("--nopayload")
                    .arg(pkg.join(component).with_extension("pkg"));
                builder.run(&mut cmd);
            };

            let prepare = |name: &str| {
                builder.create_dir(&pkg.join(name));
                builder.cp_r(&work.join(&format!("{}-{}", pkgname(builder, name), target)),
                        &pkg.join(name));
                builder.install(&etc.join("pkg/postinstall"), &pkg.join(name), 0o755);
                pkgbuild(name);
            };
            prepare("rustc");
            prepare("cargo");
            prepare("rust-docs");
            prepare("rust-std");
            prepare("rust-analysis");

            if rls_installer.is_some() {
                prepare("rls");
            }
            if clippy_installer.is_some() {
                prepare("clippy");
            }

            // create an 'uninstall' package
            builder.install(&etc.join("pkg/postinstall"), &pkg.join("uninstall"), 0o755);
            pkgbuild("uninstall");

            builder.create_dir(&pkg.join("res"));
            builder.create(&pkg.join("res/LICENSE.txt"), &license);
            builder.install(&etc.join("gfx/rust-logo.png"), &pkg.join("res"), 0o644);
            let mut cmd = Command::new("productbuild");
            cmd.arg("--distribution").arg(xform(&etc.join("pkg/Distribution.xml")))
                .arg("--resources").arg(pkg.join("res"))
                .arg(distdir(builder).join(format!("{}-{}.pkg",
                                                    pkgname(builder, "rust"),
                                                    target)))
                .arg("--package-path").arg(&pkg);
            builder.run(&mut cmd);
        }

        if target.contains("windows") {
            let exe = tmp.join("exe");
            let _ = fs::remove_dir_all(&exe);

            let prepare = |name: &str| {
                builder.create_dir(&exe.join(name));
                let dir = if name == "rust-std" || name == "rust-analysis" {
                    format!("{}-{}", name, target)
                } else if name == "rls" {
                    "rls-preview".to_string()
                } else if name == "clippy" {
                    "clippy-preview".to_string()
                } else {
                    name.to_string()
                };
                builder.cp_r(&work.join(&format!("{}-{}", pkgname(builder, name), target))
                            .join(dir),
                        &exe.join(name));
                builder.remove(&exe.join(name).join("manifest.in"));
            };
            prepare("rustc");
            prepare("cargo");
            prepare("rust-analysis");
            prepare("rust-docs");
            prepare("rust-std");
            if rls_installer.is_some() {
                prepare("rls");
            }
            if clippy_installer.is_some() {
                prepare("clippy");
            }
            if target.contains("windows-gnu") {
                prepare("rust-mingw");
            }

            builder.install(&xform(&etc.join("exe/rust.iss")), &exe, 0o644);
            builder.install(&etc.join("exe/modpath.iss"), &exe, 0o644);
            builder.install(&etc.join("exe/upgrade.iss"), &exe, 0o644);
            builder.install(&etc.join("gfx/rust-logo.ico"), &exe, 0o644);
            builder.create(&exe.join("LICENSE.txt"), &license);

            // Generate exe installer
            let mut cmd = Command::new("iscc");
            cmd.arg("rust.iss")
                .current_dir(&exe);
            if target.contains("windows-gnu") {
                cmd.arg("/dMINGW");
            }
            add_env(builder, &mut cmd, target);
            builder.run(&mut cmd);
            builder.install(&exe.join(format!("{}-{}.exe", pkgname(builder, "rust"), target)),
                    &distdir(builder),
                    0o755);

            // Generate msi installer
            let wix = PathBuf::from(env::var_os("WIX").unwrap());
            let heat = wix.join("bin/heat.exe");
            let candle = wix.join("bin/candle.exe");
            let light = wix.join("bin/light.exe");

            let heat_flags = ["-nologo", "-gg", "-sfrag", "-srd", "-sreg"];
            builder.run(Command::new(&heat)
                            .current_dir(&exe)
                            .arg("dir")
                            .arg("rustc")
                            .args(&heat_flags)
                            .arg("-cg").arg("RustcGroup")
                            .arg("-dr").arg("Rustc")
                            .arg("-var").arg("var.RustcDir")
                            .arg("-out").arg(exe.join("RustcGroup.wxs")));
            builder.run(Command::new(&heat)
                            .current_dir(&exe)
                            .arg("dir")
                            .arg("rust-docs")
                            .args(&heat_flags)
                            .arg("-cg").arg("DocsGroup")
                            .arg("-dr").arg("Docs")
                            .arg("-var").arg("var.DocsDir")
                            .arg("-out").arg(exe.join("DocsGroup.wxs"))
                            .arg("-t").arg(etc.join("msi/squash-components.xsl")));
            builder.run(Command::new(&heat)
                            .current_dir(&exe)
                            .arg("dir")
                            .arg("cargo")
                            .args(&heat_flags)
                            .arg("-cg").arg("CargoGroup")
                            .arg("-dr").arg("Cargo")
                            .arg("-var").arg("var.CargoDir")
                            .arg("-out").arg(exe.join("CargoGroup.wxs"))
                            .arg("-t").arg(etc.join("msi/remove-duplicates.xsl")));
            builder.run(Command::new(&heat)
                            .current_dir(&exe)
                            .arg("dir")
                            .arg("rust-std")
                            .args(&heat_flags)
                            .arg("-cg").arg("StdGroup")
                            .arg("-dr").arg("Std")
                            .arg("-var").arg("var.StdDir")
                            .arg("-out").arg(exe.join("StdGroup.wxs")));
            if rls_installer.is_some() {
                builder.run(Command::new(&heat)
                                .current_dir(&exe)
                                .arg("dir")
                                .arg("rls")
                                .args(&heat_flags)
                                .arg("-cg").arg("RlsGroup")
                                .arg("-dr").arg("Rls")
                                .arg("-var").arg("var.RlsDir")
                                .arg("-out").arg(exe.join("RlsGroup.wxs"))
                                .arg("-t").arg(etc.join("msi/remove-duplicates.xsl")));
            }
            if clippy_installer.is_some() {
                builder.run(Command::new(&heat)
                                .current_dir(&exe)
                                .arg("dir")
                                .arg("clippy")
                                .args(&heat_flags)
                                .arg("-cg").arg("ClippyGroup")
                                .arg("-dr").arg("Clippy")
                                .arg("-var").arg("var.ClippyDir")
                                .arg("-out").arg(exe.join("ClippyGroup.wxs"))
                                .arg("-t").arg(etc.join("msi/remove-duplicates.xsl")));
            }
            builder.run(Command::new(&heat)
                            .current_dir(&exe)
                            .arg("dir")
                            .arg("rust-analysis")
                            .args(&heat_flags)
                            .arg("-cg").arg("AnalysisGroup")
                            .arg("-dr").arg("Analysis")
                            .arg("-var").arg("var.AnalysisDir")
                            .arg("-out").arg(exe.join("AnalysisGroup.wxs"))
                            .arg("-t").arg(etc.join("msi/remove-duplicates.xsl")));
            if target.contains("windows-gnu") {
                builder.run(Command::new(&heat)
                                .current_dir(&exe)
                                .arg("dir")
                                .arg("rust-mingw")
                                .args(&heat_flags)
                                .arg("-cg").arg("GccGroup")
                                .arg("-dr").arg("Gcc")
                                .arg("-var").arg("var.GccDir")
                                .arg("-out").arg(exe.join("GccGroup.wxs")));
            }

            let candle = |input: &Path| {
                let output = exe.join(input.file_stem().unwrap())
                                .with_extension("wixobj");
                let arch = if target.contains("x86_64") {"x64"} else {"x86"};
                let mut cmd = Command::new(&candle);
                cmd.current_dir(&exe)
                    .arg("-nologo")
                    .arg("-dRustcDir=rustc")
                    .arg("-dDocsDir=rust-docs")
                    .arg("-dCargoDir=cargo")
                    .arg("-dStdDir=rust-std")
                    .arg("-dAnalysisDir=rust-analysis")
                    .arg("-arch").arg(&arch)
                    .arg("-out").arg(&output)
                    .arg(&input);
                add_env(builder, &mut cmd, target);

                if rls_installer.is_some() {
                    cmd.arg("-dRlsDir=rls");
                }
                if clippy_installer.is_some() {
                    cmd.arg("-dClippyDir=clippy");
                }
                if target.contains("windows-gnu") {
                    cmd.arg("-dGccDir=rust-mingw");
                }
                builder.run(&mut cmd);
            };
            candle(&xform(&etc.join("msi/rust.wxs")));
            candle(&etc.join("msi/ui.wxs"));
            candle(&etc.join("msi/rustwelcomedlg.wxs"));
            candle("RustcGroup.wxs".as_ref());
            candle("DocsGroup.wxs".as_ref());
            candle("CargoGroup.wxs".as_ref());
            candle("StdGroup.wxs".as_ref());
            if rls_installer.is_some() {
                candle("RlsGroup.wxs".as_ref());
            }
            if clippy_installer.is_some() {
                candle("ClippyGroup.wxs".as_ref());
            }
            candle("AnalysisGroup.wxs".as_ref());

            if target.contains("windows-gnu") {
                candle("GccGroup.wxs".as_ref());
            }

            builder.create(&exe.join("LICENSE.rtf"), &rtf);
            builder.install(&etc.join("gfx/banner.bmp"), &exe, 0o644);
            builder.install(&etc.join("gfx/dialogbg.bmp"), &exe, 0o644);

            let filename = format!("{}-{}.msi", pkgname(builder, "rust"), target);
            let mut cmd = Command::new(&light);
            cmd.arg("-nologo")
                .arg("-ext").arg("WixUIExtension")
                .arg("-ext").arg("WixUtilExtension")
                .arg("-out").arg(exe.join(&filename))
                .arg("rust.wixobj")
                .arg("ui.wixobj")
                .arg("rustwelcomedlg.wixobj")
                .arg("RustcGroup.wixobj")
                .arg("DocsGroup.wixobj")
                .arg("CargoGroup.wixobj")
                .arg("StdGroup.wixobj")
                .arg("AnalysisGroup.wixobj")
                .current_dir(&exe);

            if rls_installer.is_some() {
                cmd.arg("RlsGroup.wixobj");
            }
            if clippy_installer.is_some() {
                cmd.arg("ClippyGroup.wixobj");
            }

            if target.contains("windows-gnu") {
                cmd.arg("GccGroup.wixobj");
            }
            // ICE57 wrongly complains about the shortcuts
            cmd.arg("-sice:ICE57");

            builder.run(&mut cmd);

            if !builder.config.dry_run {
                t!(fs::rename(exe.join(&filename), distdir(builder).join(&filename)));
            }
        }
    }
}

fn add_env(builder: &Builder, cmd: &mut Command, target: Interned<String>) {
    let mut parts = channel::CFG_RELEASE_NUM.split('.');
    cmd.env("CFG_RELEASE_INFO", builder.rust_version())
       .env("CFG_RELEASE_NUM", channel::CFG_RELEASE_NUM)
       .env("CFG_RELEASE", builder.rust_release())
       .env("CFG_VER_MAJOR", parts.next().unwrap())
       .env("CFG_VER_MINOR", parts.next().unwrap())
       .env("CFG_VER_PATCH", parts.next().unwrap())
       .env("CFG_VER_BUILD", "0") // just needed to build
       .env("CFG_PACKAGE_VERS", builder.rust_package_vers())
       .env("CFG_PACKAGE_NAME", pkgname(builder, "rust"))
       .env("CFG_BUILD", target)
       .env("CFG_CHANNEL", &builder.config.channel);

    if target.contains("windows-gnu") {
       cmd.env("CFG_MINGW", "1")
          .env("CFG_ABI", "GNU");
    } else {
       cmd.env("CFG_MINGW", "0")
          .env("CFG_ABI", "MSVC");
    }

    if target.contains("x86_64") {
       cmd.env("CFG_PLATFORM", "x64");
    } else {
       cmd.env("CFG_PLATFORM", "x86");
    }
}

#[derive(Debug, PartialOrd, Ord, Copy, Clone, Hash, PartialEq, Eq)]
pub struct HashSign;

impl Step for HashSign {
    type Output = ();
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("hash-and-sign")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(HashSign);
    }

    fn run(self, builder: &Builder) {
        let mut cmd = builder.tool_cmd(Tool::BuildManifest);
        if builder.config.dry_run {
            return;
        }
        let sign = builder.config.dist_sign_folder.as_ref().unwrap_or_else(|| {
            panic!("\n\nfailed to specify `dist.sign-folder` in `config.toml`\n\n")
        });
        let addr = builder.config.dist_upload_addr.as_ref().unwrap_or_else(|| {
            panic!("\n\nfailed to specify `dist.upload-addr` in `config.toml`\n\n")
        });
        let file = builder.config.dist_gpg_password_file.as_ref().unwrap_or_else(|| {
            panic!("\n\nfailed to specify `dist.gpg-password-file` in `config.toml`\n\n")
        });
        let mut pass = String::new();
        t!(t!(File::open(&file)).read_to_string(&mut pass));

        let today = output(Command::new("date").arg("+%Y-%m-%d"));

        cmd.arg(sign);
        cmd.arg(distdir(builder));
        cmd.arg(today.trim());
        cmd.arg(builder.rust_package_vers());
        cmd.arg(builder.package_vers(&builder.release_num("cargo")));
        cmd.arg(builder.package_vers(&builder.release_num("rls")));
        cmd.arg(builder.package_vers(&builder.release_num("clippy")));
        cmd.arg(builder.package_vers(&builder.release_num("rustfmt")));
        cmd.arg(builder.llvm_tools_package_vers());
        cmd.arg(builder.lldb_package_vers());
        cmd.arg(addr);

        builder.create_dir(&distdir(builder));

        let mut child = t!(cmd.stdin(Stdio::piped()).spawn());
        t!(child.stdin.take().unwrap().write_all(pass.as_bytes()));
        let status = t!(child.wait());
        assert!(status.success());
    }
}

// Maybe add libLLVM.so to the lib-dir. It will only have been built if
// LLVM tools are linked dynamically.
// Note: This function does no yet support Windows but we also don't support
//       linking LLVM tools dynamically on Windows yet.
fn maybe_install_llvm_dylib(builder: &Builder,
                            target: Interned<String>,
                            image: &Path) {
    let src_libdir = builder
        .llvm_out(target)
        .join("lib");
    let dst_libdir = image.join("lib/rustlib").join(&*target).join("lib");
    t!(fs::create_dir_all(&dst_libdir));

    if target.contains("apple-darwin") {
        let llvm_dylib_path = src_libdir.join("libLLVM.dylib");
        if llvm_dylib_path.exists() {
            builder.install(&llvm_dylib_path, &dst_libdir, 0o644);
        }
        return
    }

    // Usually libLLVM.so is a symlink to something like libLLVM-6.0.so.
    // Since tools link to the latter rather than the former, we have to
    // follow the symlink to find out what to distribute.
    let llvm_dylib_path = src_libdir.join("libLLVM.so");
    if llvm_dylib_path.exists() {
        let llvm_dylib_path = llvm_dylib_path.canonicalize().unwrap_or_else(|e| {
            panic!("dist: Error calling canonicalize path `{}`: {}",
                   llvm_dylib_path.display(), e);
        });


        builder.install(&llvm_dylib_path, &dst_libdir, 0o644);
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct LlvmTools {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for LlvmTools {
    type Output = Option<PathBuf>;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("llvm-tools")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(LlvmTools {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let stage = self.stage;
        let target = self.target;
        assert!(builder.config.extended);

        /* run only if llvm-config isn't used */
        if let Some(config) = builder.config.target_config.get(&target) {
            if let Some(ref _s) = config.llvm_config {
                builder.info(&format!("Skipping LlvmTools stage{} ({}): external LLVM",
                    stage, target));
                return None;
            }
        }

        builder.info(&format!("Dist LlvmTools stage{} ({})", stage, target));
        let src = builder.src.join("src/llvm");
        let name = pkgname(builder, "llvm-tools");

        let tmp = tmpdir(builder);
        let image = tmp.join("llvm-tools-image");
        drop(fs::remove_dir_all(&image));

        // Prepare the image directory
        let src_bindir = builder
            .llvm_out(target)
            .join("bin");
        let dst_bindir = image.join("lib/rustlib")
            .join(&*target)
            .join("bin");
        t!(fs::create_dir_all(&dst_bindir));
        for tool in LLVM_TOOLS {
            let exe = src_bindir.join(exe(tool, &target));
            builder.install(&exe, &dst_bindir, 0o755);
        }

        // Prepare the overlay
        let overlay = tmp.join("llvm-tools-overlay");
        drop(fs::remove_dir_all(&overlay));
        builder.create_dir(&overlay);
        builder.install(&src.join("README.txt"), &overlay, 0o644);
        builder.install(&src.join("LICENSE.TXT"), &overlay, 0o644);
        builder.create(&overlay.join("version"), &builder.llvm_tools_vers());

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
            .arg("--product-name=Rust")
            .arg("--rel-manifest-dir=rustlib")
            .arg("--success-message=llvm-tools-installed.")
            .arg("--image-dir").arg(&image)
            .arg("--work-dir").arg(&tmpdir(builder))
            .arg("--output-dir").arg(&distdir(builder))
            .arg("--non-installed-overlay").arg(&overlay)
            .arg(format!("--package-name={}-{}", name, target))
            .arg("--legacy-manifest-dirs=rustlib,cargo")
            .arg("--component-name=llvm-tools-preview");


        builder.run(&mut cmd);
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, target)))
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Lldb {
    pub target: Interned<String>,
}

impl Step for Lldb {
    type Output = Option<PathBuf>;
    const ONLY_HOSTS: bool = true;
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.path("src/tools/lldb")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Lldb {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) -> Option<PathBuf> {
        let target = self.target;

        if builder.config.dry_run {
            return None;
        }

        let bindir = builder
            .llvm_out(target)
            .join("bin");
        let lldb_exe = bindir.join(exe("lldb", &target));
        if !lldb_exe.exists() {
            return None;
        }

        builder.info(&format!("Dist Lldb ({})", target));
        let src = builder.src.join("src/tools/lldb");
        let name = pkgname(builder, "lldb");

        let tmp = tmpdir(builder);
        let image = tmp.join("lldb-image");
        drop(fs::remove_dir_all(&image));

        // Prepare the image directory
        let dst = image.join("bin");
        t!(fs::create_dir_all(&dst));
        for program in &["lldb", "lldb-argdumper", "lldb-mi", "lldb-server"] {
            let exe = bindir.join(exe(program, &target));
            builder.install(&exe, &dst, 0o755);
        }

        // The libraries.
        let libdir = builder.llvm_out(target).join("lib");
        let dst = image.join("lib");
        t!(fs::create_dir_all(&dst));
        for entry in t!(fs::read_dir(&libdir)) {
            let entry = entry.unwrap();
            if let Ok(name) = entry.file_name().into_string() {
                if name.starts_with("liblldb.") && !name.ends_with(".a") {
                    if t!(entry.file_type()).is_symlink() {
                        builder.copy_to_folder(&entry.path(), &dst);
                    } else {
                       builder.install(&entry.path(), &dst, 0o755);
                    }
                }
            }
        }

        // The lldb scripts might be installed in lib/python$version
        // or in lib64/python$version.  If lib64 exists, use it;
        // otherwise lib.
        let libdir = builder.llvm_out(target).join("lib64");
        let (libdir, libdir_name) = if libdir.exists() {
            (libdir, "lib64")
        } else {
            (builder.llvm_out(target).join("lib"), "lib")
        };
        for entry in t!(fs::read_dir(&libdir)) {
            let entry = t!(entry);
            if let Ok(name) = entry.file_name().into_string() {
                if name.starts_with("python") {
                    let dst = image.join(libdir_name)
                        .join(entry.file_name());
                    t!(fs::create_dir_all(&dst));
                    builder.cp_r(&entry.path(), &dst);
                    break;
                }
            }
        }

        // Prepare the overlay
        let overlay = tmp.join("lldb-overlay");
        drop(fs::remove_dir_all(&overlay));
        builder.create_dir(&overlay);
        builder.install(&src.join("LICENSE.TXT"), &overlay, 0o644);
        builder.create(&overlay.join("version"), &builder.lldb_vers());

        // Generate the installer tarball
        let mut cmd = rust_installer(builder);
        cmd.arg("generate")
            .arg("--product-name=Rust")
            .arg("--rel-manifest-dir=rustlib")
            .arg("--success-message=lldb-installed.")
            .arg("--image-dir").arg(&image)
            .arg("--work-dir").arg(&tmpdir(builder))
            .arg("--output-dir").arg(&distdir(builder))
            .arg("--non-installed-overlay").arg(&overlay)
            .arg(format!("--package-name={}-{}", name, target))
            .arg("--legacy-manifest-dirs=rustlib,cargo")
            .arg("--component-name=lldb-preview");


        builder.run(&mut cmd);
        Some(distdir(builder).join(format!("{}-{}.tar.gz", name, target)))
    }
}
// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Documentation generation for rustbuilder.
//
// This module implements generation for all bits and pieces of documentation
// for the Rust project. This notably includes suites like the rust book, the
// nomicon, rust by example, standalone documentation, etc.
//
// Everything here is basically just a shim around calling either `rustbook` or
// `rustdoc`.

use std::collections::HashSet;
use std::fs::{self, File};
use std::io::prelude::*;
use std::io;
use std::path::{PathBuf, Path};

use Mode;
use build_helper::up_to_date;

use util::symlink_dir;
use builder::{Builder, Compiler, RunConfig, ShouldRun, Step};
use tool::{self, prepare_tool_cargo, Tool, SourceType};
use compile;
use cache::{INTERNER, Interned};
use config::Config;

macro_rules! book {
    ($($name:ident, $path:expr, $book_name:expr;)+) => {
        $(
            #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
        pub struct $name {
            target: Interned<String>,
        }

        impl Step for $name {
            type Output = ();
            const DEFAULT: bool = true;

            fn should_run(run: ShouldRun) -> ShouldRun {
                let builder = run.builder;
                run.path($path).default_condition(builder.config.docs)
            }

            fn make_run(run: RunConfig) {
                run.builder.ensure($name {
                    target: run.target,
                });
            }

            fn run(self, builder: &Builder) {
                builder.ensure(Rustbook {
                    target: self.target,
                    name: INTERNER.intern_str($book_name),
                })
            }
        }
        )+
    }
}

book!(
    Nomicon, "src/doc/nomicon", "nomicon";
    Reference, "src/doc/reference", "reference";
    RustdocBook, "src/doc/rustdoc", "rustdoc";
    RustcBook, "src/doc/rustc", "rustc";
    RustByExample, "src/doc/rust-by-example", "rust-by-example";
);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
struct Rustbook {
    target: Interned<String>,
    name: Interned<String>,
}

impl Step for Rustbook {
    type Output = ();

    // rustbook is never directly called, and only serves as a shim for the nomicon and the
    // reference.
    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Invoke `rustbook` for `target` for the doc book `name`.
    ///
    /// This will not actually generate any documentation if the documentation has
    /// already been generated.
    fn run(self, builder: &Builder) {
        let src = builder.src.join("src/doc");
        builder.ensure(RustbookSrc {
            target: self.target,
            name: self.name,
            src: INTERNER.intern_path(src),
        });
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct UnstableBook {
    target: Interned<String>,
}

impl Step for UnstableBook {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/doc/unstable-book").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(UnstableBook {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) {
        builder.ensure(UnstableBookGen {
            target: self.target,
        });
        builder.ensure(RustbookSrc {
            target: self.target,
            name: INTERNER.intern_str("unstable-book"),
            src: builder.md_doc_out(self.target),
        })
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct CargoBook {
    target: Interned<String>,
    name: Interned<String>,
}

impl Step for CargoBook {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/tools/cargo/src/doc/book").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(CargoBook {
            target: run.target,
            name: INTERNER.intern_str("cargo"),
        });
    }

    fn run(self, builder: &Builder) {
        let target = self.target;
        let name = self.name;
        let src = builder.src.join("src/tools/cargo/src/doc");

        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));

        let out = out.join(name);

        builder.info(&format!("Cargo Book ({}) - {}", target, name));

        let _ = fs::remove_dir_all(&out);

        builder.run(builder.tool_cmd(Tool::Rustbook)
                       .arg("build")
                       .arg(&src)
                       .arg("-d")
                       .arg(out));
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
struct RustbookSrc {
    target: Interned<String>,
    name: Interned<String>,
    src: Interned<PathBuf>,
}

impl Step for RustbookSrc {
    type Output = ();

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.never()
    }

    /// Invoke `rustbook` for `target` for the doc book `name` from the `src` path.
    ///
    /// This will not actually generate any documentation if the documentation has
    /// already been generated.
    fn run(self, builder: &Builder) {
        let target = self.target;
        let name = self.name;
        let src = self.src;
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));

        let out = out.join(name);
        let src = src.join(name);
        let index = out.join("index.html");
        let rustbook = builder.tool_exe(Tool::Rustbook);
        let mut rustbook_cmd = builder.tool_cmd(Tool::Rustbook);
        if up_to_date(&src, &index) && up_to_date(&rustbook, &index) {
            return
        }
        builder.info(&format!("Rustbook ({}) - {}", target, name));
        let _ = fs::remove_dir_all(&out);
        builder.run(rustbook_cmd
                       .arg("build")
                       .arg(&src)
                       .arg("-d")
                       .arg(out));
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct TheBook {
    compiler: Compiler,
    target: Interned<String>,
    name: &'static str,
}

impl Step for TheBook {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/doc/book").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(TheBook {
            compiler: run.builder.compiler(run.builder.top_stage, run.builder.config.build),
            target: run.target,
            name: "book",
        });
    }

    /// Build the book and associated stuff.
    ///
    /// We need to build:
    ///
    /// * Book (first edition)
    /// * Book (second edition)
    /// * Version info and CSS
    /// * Index page
    /// * Redirect pages
    fn run(self, builder: &Builder) {
        let compiler = self.compiler;
        let target = self.target;
        let name = self.name;
        // build book first edition
        builder.ensure(Rustbook {
            target,
            name: INTERNER.intern_string(format!("{}/first-edition", name)),
        });

        // build book second edition
        builder.ensure(Rustbook {
            target,
            name: INTERNER.intern_string(format!("{}/second-edition", name)),
        });

        // build book 2018 edition
        builder.ensure(Rustbook {
            target,
            name: INTERNER.intern_string(format!("{}/2018-edition", name)),
        });

        // build the version info page and CSS
        builder.ensure(Standalone {
            compiler,
            target,
        });

        // build the index page
        let index = format!("{}/index.md", name);
        builder.info(&format!("Documenting book index ({})", target));
        invoke_rustdoc(builder, compiler, target, &index);

        // build the redirect pages
        builder.info(&format!("Documenting book redirect pages ({})", target));
        for file in t!(fs::read_dir(builder.src.join("src/doc/book/redirects"))) {
            let file = t!(file);
            let path = file.path();
            let path = path.to_str().unwrap();

            invoke_rustdoc(builder, compiler, target, path);
        }
    }
}

fn invoke_rustdoc(builder: &Builder, compiler: Compiler, target: Interned<String>, markdown: &str) {
    let out = builder.doc_out(target);

    let path = builder.src.join("src/doc").join(markdown);

    let favicon = builder.src.join("src/doc/favicon.inc");
    let footer = builder.src.join("src/doc/footer.inc");
    let version_info = out.join("version_info.html");

    let mut cmd = builder.rustdoc_cmd(compiler.host);

    let out = out.join("book");

    cmd.arg("--html-after-content").arg(&footer)
        .arg("--html-before-content").arg(&version_info)
        .arg("--html-in-header").arg(&favicon)
        .arg("--markdown-no-toc")
        .arg("--markdown-playground-url")
        .arg("https://play.rust-lang.org/")
        .arg("-o").arg(&out)
        .arg(&path)
        .arg("--markdown-css")
        .arg("../rust.css");

    builder.run(&mut cmd);
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Standalone {
    compiler: Compiler,
    target: Interned<String>,
}

impl Step for Standalone {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/doc").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Standalone {
            compiler: run.builder.compiler(run.builder.top_stage, run.builder.config.build),
            target: run.target,
        });
    }

    /// Generates all standalone documentation as compiled by the rustdoc in `stage`
    /// for the `target` into `out`.
    ///
    /// This will list all of `src/doc` looking for markdown files and appropriately
    /// perform transformations like substituting `VERSION`, `SHORT_HASH`, and
    /// `STAMP` along with providing the various header/footer HTML we've customized.
    ///
    /// In the end, this is just a glorified wrapper around rustdoc!
    fn run(self, builder: &Builder) {
        let target = self.target;
        let compiler = self.compiler;
        builder.info(&format!("Documenting standalone ({})", target));
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));

        let favicon = builder.src.join("src/doc/favicon.inc");
        let footer = builder.src.join("src/doc/footer.inc");
        let full_toc = builder.src.join("src/doc/full-toc.inc");
        t!(fs::copy(builder.src.join("src/doc/rust.css"), out.join("rust.css")));

        let version_input = builder.src.join("src/doc/version_info.html.template");
        let version_info = out.join("version_info.html");

        if !builder.config.dry_run && !up_to_date(&version_input, &version_info) {
            let mut info = String::new();
            t!(t!(File::open(&version_input)).read_to_string(&mut info));
            let info = info.replace("VERSION", &builder.rust_release())
                           .replace("SHORT_HASH", builder.rust_info.sha_short().unwrap_or(""))
                           .replace("STAMP", builder.rust_info.sha().unwrap_or(""));
            t!(t!(File::create(&version_info)).write_all(info.as_bytes()));
        }

        for file in t!(fs::read_dir(builder.src.join("src/doc"))) {
            let file = t!(file);
            let path = file.path();
            let filename = path.file_name().unwrap().to_str().unwrap();
            if !filename.ends_with(".md") || filename == "README.md" {
                continue
            }

            let html = out.join(filename).with_extension("html");
            let rustdoc = builder.rustdoc(compiler.host);
            if up_to_date(&path, &html) &&
               up_to_date(&footer, &html) &&
               up_to_date(&favicon, &html) &&
               up_to_date(&full_toc, &html) &&
               up_to_date(&version_info, &html) &&
               (builder.config.dry_run || up_to_date(&rustdoc, &html)) {
                continue
            }

            let mut cmd = builder.rustdoc_cmd(compiler.host);
            cmd.arg("--html-after-content").arg(&footer)
               .arg("--html-before-content").arg(&version_info)
               .arg("--html-in-header").arg(&favicon)
               .arg("--markdown-playground-url")
               .arg("https://play.rust-lang.org/")
               .arg("-o").arg(&out)
               .arg(&path);

            if filename == "not_found.md" {
                cmd.arg("--markdown-no-toc")
                   .arg("--markdown-css")
                   .arg("https://doc.rust-lang.org/rust.css");
            } else {
                cmd.arg("--markdown-css").arg("rust.css");
            }
            builder.run(&mut cmd);
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Std {
    pub stage: u32,
    pub target: Interned<String>,
}

impl Step for Std {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.all_krates("std").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Std {
            stage: run.builder.top_stage,
            target: run.target
        });
    }

    /// Compile all standard library documentation.
    ///
    /// This will generate all documentation for the standard library and its
    /// dependencies. This is largely just a wrapper around `cargo doc`.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;
        builder.info(&format!("Documenting stage{} std ({})", stage, target));
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));
        let compiler = builder.compiler(stage, builder.config.build);
        let rustdoc = builder.rustdoc(compiler.host);
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler
        };

        builder.ensure(compile::Std { compiler, target });
        let out_dir = builder.stage_out(compiler, Mode::Std)
                           .join(target).join("doc");

        // Here what we're doing is creating a *symlink* (directory junction on
        // Windows) to the final output location. This is not done as an
        // optimization but rather for correctness. We've got three trees of
        // documentation, one for std, one for test, and one for rustc. It's then
        // our job to merge them all together.
        //
        // Unfortunately rustbuild doesn't know nearly as well how to merge doc
        // trees as rustdoc does itself, so instead of actually having three
        // separate trees we just have rustdoc output to the same location across
        // all of them.
        //
        // This way rustdoc generates output directly into the output, and rustdoc
        // will also directly handle merging.
        let my_out = builder.crate_doc_out(target);
        builder.clear_if_dirty(&my_out, &rustdoc);
        t!(symlink_dir_force(&builder.config, &my_out, &out_dir));

        let mut cargo = builder.cargo(compiler, Mode::Std, target, "doc");
        compile::std_cargo(builder, &compiler, target, &mut cargo);

        // Keep a whitelist so we do not build internal stdlib crates, these will be
        // build by the rustc step later if enabled.
        cargo.arg("--no-deps");
        for krate in &["alloc", "core", "std"] {
            cargo.arg("-p").arg(krate);
            // Create all crate output directories first to make sure rustdoc uses
            // relative links.
            // FIXME: Cargo should probably do this itself.
            t!(fs::create_dir_all(out_dir.join(krate)));
        }

        builder.run(&mut cargo);
        builder.cp_r(&my_out, &out);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Test {
    stage: u32,
    target: Interned<String>,
}

impl Step for Test {
    type Output = ();
    const DEFAULT: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.krate("test").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Test {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    /// Compile all libtest documentation.
    ///
    /// This will generate all documentation for libtest and its dependencies. This
    /// is largely just a wrapper around `cargo doc`.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;
        builder.info(&format!("Documenting stage{} test ({})", stage, target));
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));
        let compiler = builder.compiler(stage, builder.config.build);
        let rustdoc = builder.rustdoc(compiler.host);
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler
        };

        // Build libstd docs so that we generate relative links
        builder.ensure(Std { stage, target });

        builder.ensure(compile::Test { compiler, target });
        let out_dir = builder.stage_out(compiler, Mode::Test)
                           .join(target).join("doc");

        // See docs in std above for why we symlink
        let my_out = builder.crate_doc_out(target);
        builder.clear_if_dirty(&my_out, &rustdoc);
        t!(symlink_dir_force(&builder.config, &my_out, &out_dir));

        let mut cargo = builder.cargo(compiler, Mode::Test, target, "doc");
        compile::test_cargo(builder, &compiler, target, &mut cargo);

        cargo.arg("--no-deps").arg("-p").arg("test");

        builder.run(&mut cargo);
        builder.cp_r(&my_out, &out);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct WhitelistedRustc {
    stage: u32,
    target: Interned<String>,
}

impl Step for WhitelistedRustc {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.krate("rustc-main").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(WhitelistedRustc {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    /// Generate whitelisted compiler crate documentation.
    ///
    /// This will generate all documentation for crates that are whitelisted
    /// to be included in the standard documentation. This documentation is
    /// included in the standard Rust documentation, so we should always
    /// document it and symlink to merge with the rest of the std and test
    /// documentation. We don't build other compiler documentation
    /// here as we want to be able to keep it separate from the standard
    /// documentation. This is largely just a wrapper around `cargo doc`.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;
        builder.info(&format!("Documenting stage{} whitelisted compiler ({})", stage, target));
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));
        let compiler = builder.compiler(stage, builder.config.build);
        let rustdoc = builder.rustdoc(compiler.host);
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler
        };

        // Build libstd docs so that we generate relative links
        builder.ensure(Std { stage, target });

        builder.ensure(compile::Rustc { compiler, target });
        let out_dir = builder.stage_out(compiler, Mode::Rustc)
                           .join(target).join("doc");

        // See docs in std above for why we symlink
        let my_out = builder.crate_doc_out(target);
        builder.clear_if_dirty(&my_out, &rustdoc);
        t!(symlink_dir_force(&builder.config, &my_out, &out_dir));

        let mut cargo = builder.cargo(compiler, Mode::Rustc, target, "doc");
        compile::rustc_cargo(builder, &mut cargo);

        // We don't want to build docs for internal compiler dependencies in this
        // step (there is another step for that). Therefore, we whitelist the crates
        // for which docs must be built.
        cargo.arg("--no-deps");
        for krate in &["proc_macro"] {
            cargo.arg("-p").arg(krate);
        }

        builder.run(&mut cargo);
        builder.cp_r(&my_out, &out);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Rustc {
    stage: u32,
    target: Interned<String>,
}

impl Step for Rustc {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.krate("rustc-main").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustc {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    /// Generate compiler documentation.
    ///
    /// This will generate all documentation for compiler and dependencies.
    /// Compiler documentation is distributed separately, so we make sure
    /// we do not merge it with the other documentation from std, test and
    /// proc_macros. This is largely just a wrapper around `cargo doc`.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;
        builder.info(&format!("Documenting stage{} compiler ({})", stage, target));

        // This is the intended out directory for compiler documentation.
        let out = builder.compiler_doc_out(target);
        t!(fs::create_dir_all(&out));

        // Get the correct compiler for this stage.
        let compiler = builder.compiler(stage, builder.config.build);
        let rustdoc = builder.rustdoc(compiler.host);
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler
        };

        if !builder.config.compiler_docs {
            builder.info("\tskipping - compiler/librustdoc docs disabled");
            return;
        }

        // Build libstd docs so that we generate relative links.
        builder.ensure(Std { stage, target });

        // Build rustc.
        builder.ensure(compile::Rustc { compiler, target });

        // We do not symlink to the same shared folder that already contains std library
        // documentation from previous steps as we do not want to include that.
        let out_dir = builder.stage_out(compiler, Mode::Rustc).join(target).join("doc");
        builder.clear_if_dirty(&out, &rustdoc);
        t!(symlink_dir_force(&builder.config, &out, &out_dir));

        // Build cargo command.
        let mut cargo = builder.cargo(compiler, Mode::Rustc, target, "doc");
        cargo.env("RUSTDOCFLAGS", "--document-private-items");
        compile::rustc_cargo(builder, &mut cargo);

        // Only include compiler crates, no dependencies of those, such as `libc`.
        cargo.arg("--no-deps");

        // Find dependencies for top level crates.
        let mut compiler_crates = HashSet::new();
        for root_crate in &["rustc", "rustc_driver", "rustc_codegen_llvm"] {
            let interned_root_crate = INTERNER.intern_str(root_crate);
            find_compiler_crates(builder, &interned_root_crate, &mut compiler_crates);
        }

        for krate in &compiler_crates {
            cargo.arg("-p").arg(krate);
        }

        builder.run(&mut cargo);
    }
}

fn find_compiler_crates(
    builder: &Builder,
    name: &Interned<String>,
    crates: &mut HashSet<Interned<String>>
) {
    // Add current crate.
    crates.insert(*name);

    // Look for dependencies.
    for dep in builder.crates.get(name).unwrap().deps.iter() {
        if builder.crates.get(dep).unwrap().is_local(builder) {
            find_compiler_crates(builder, dep, crates);
        }
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Rustdoc {
    stage: u32,
    target: Interned<String>,
}

impl Step for Rustdoc {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        run.krate("rustdoc-tool")
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Rustdoc {
            stage: run.builder.top_stage,
            target: run.target,
        });
    }

    /// Generate compiler documentation.
    ///
    /// This will generate all documentation for compiler and dependencies.
    /// Compiler documentation is distributed separately, so we make sure
    /// we do not merge it with the other documentation from std, test and
    /// proc_macros. This is largely just a wrapper around `cargo doc`.
    fn run(self, builder: &Builder) {
        let stage = self.stage;
        let target = self.target;
        builder.info(&format!("Documenting stage{} rustdoc ({})", stage, target));

        // This is the intended out directory for compiler documentation.
        let out = builder.compiler_doc_out(target);
        t!(fs::create_dir_all(&out));

        // Get the correct compiler for this stage.
        let compiler = builder.compiler(stage, builder.config.build);
        let rustdoc = builder.rustdoc(compiler.host);
        let compiler = if builder.force_use_stage1(compiler, target) {
            builder.compiler(1, compiler.host)
        } else {
            compiler
        };

        if !builder.config.compiler_docs {
            builder.info("\tskipping - compiler/librustdoc docs disabled");
            return;
        }

        // Build libstd docs so that we generate relative links.
        builder.ensure(Std { stage, target });

        // Build rustdoc.
        builder.ensure(tool::Rustdoc { host: compiler.host });

        // Symlink compiler docs to the output directory of rustdoc documentation.
        let out_dir = builder.stage_out(compiler, Mode::ToolRustc)
            .join(target)
            .join("doc");
        t!(fs::create_dir_all(&out_dir));
        builder.clear_if_dirty(&out, &rustdoc);
        t!(symlink_dir_force(&builder.config, &out, &out_dir));

        // Build cargo command.
        let mut cargo = prepare_tool_cargo(
            builder,
            compiler,
            Mode::ToolRustc,
            target,
            "doc",
            "src/tools/rustdoc",
            SourceType::InTree,
        );

        cargo.env("RUSTDOCFLAGS", "--document-private-items");
        builder.run(&mut cargo);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct ErrorIndex {
    target: Interned<String>,
}

impl Step for ErrorIndex {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/tools/error_index_generator").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(ErrorIndex {
            target: run.target,
        });
    }

    /// Generates the HTML rendered error-index by running the
    /// `error_index_generator` tool.
    fn run(self, builder: &Builder) {
        let target = self.target;

        builder.info(&format!("Documenting error index ({})", target));
        let out = builder.doc_out(target);
        t!(fs::create_dir_all(&out));
        let mut index = builder.tool_cmd(Tool::ErrorIndex);
        index.arg("html");
        index.arg(out.join("error-index.html"));

        // FIXME: shouldn't have to pass this env var
        index.env("CFG_BUILD", &builder.config.build)
             .env("RUSTC_ERROR_METADATA_DST", builder.extended_error_dir());

        builder.run(&mut index);
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct UnstableBookGen {
    target: Interned<String>,
}

impl Step for UnstableBookGen {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let builder = run.builder;
        run.path("src/tools/unstable-book-gen").default_condition(builder.config.docs)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(UnstableBookGen {
            target: run.target,
        });
    }

    fn run(self, builder: &Builder) {
        let target = self.target;

        builder.ensure(compile::Std {
            compiler: builder.compiler(builder.top_stage, builder.config.build),
            target,
        });

        builder.info(&format!("Generating unstable book md files ({})", target));
        let out = builder.md_doc_out(target).join("unstable-book");
        builder.create_dir(&out);
        builder.remove_dir(&out);
        let mut cmd = builder.tool_cmd(Tool::UnstableBookGen);
        cmd.arg(builder.src.join("src"));
        cmd.arg(out);

        builder.run(&mut cmd);
    }
}

fn symlink_dir_force(config: &Config, src: &Path, dst: &Path) -> io::Result<()> {
    if config.dry_run {
        return Ok(());
    }
    if let Ok(m) = fs::symlink_metadata(dst) {
        if m.file_type().is_dir() {
            try!(fs::remove_dir_all(dst));
        } else {
            // handle directory junctions on windows by falling back to
            // `remove_dir`.
            try!(fs::remove_file(dst).or_else(|_| {
                fs::remove_dir(dst)
            }));
        }
    }

    symlink_dir(config, src, dst)
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Command-line interface of the rustbuild build system.
//
// This module implements the command-line parsing of the build system which
// has various flags to configure how it's run.

use std::fs;
use std::path::PathBuf;
use std::process;

use getopts::Options;

use builder::Builder;
use config::Config;
use metadata;
use {Build, DocTests};

use cache::{Interned, INTERNER};

/// Deserialized version of all flags for this compile.
pub struct Flags {
    pub verbose: usize, // number of -v args; each extra -v after the first is passed to Cargo
    pub on_fail: Option<String>,
    pub stage: Option<u32>,
    pub keep_stage: Vec<u32>,

    pub host: Vec<Interned<String>>,
    pub target: Vec<Interned<String>>,
    pub config: Option<PathBuf>,
    pub jobs: Option<u32>,
    pub cmd: Subcommand,
    pub incremental: bool,
    pub exclude: Vec<PathBuf>,
    pub rustc_error_format: Option<String>,
    pub dry_run: bool,

    // true => deny
    pub warnings: Option<bool>,
}

pub enum Subcommand {
    Build {
        paths: Vec<PathBuf>,
    },
    Check {
        paths: Vec<PathBuf>,
    },
    Doc {
        paths: Vec<PathBuf>,
    },
    Test {
        paths: Vec<PathBuf>,
        /// Whether to automatically update stderr/stdout files
        bless: bool,
        compare_mode: Option<String>,
        test_args: Vec<String>,
        rustc_args: Vec<String>,
        fail_fast: bool,
        doc_tests: DocTests,
    },
    Bench {
        paths: Vec<PathBuf>,
        test_args: Vec<String>,
    },
    Clean {
        all: bool,
    },
    Dist {
        paths: Vec<PathBuf>,
    },
    Install {
        paths: Vec<PathBuf>,
    },
}

impl Default for Subcommand {
    fn default() -> Subcommand {
        Subcommand::Build {
            paths: vec![PathBuf::from("nowhere")],
        }
    }
}

impl Flags {
    pub fn parse(args: &[String]) -> Flags {
        let mut extra_help = String::new();
        let mut subcommand_help = format!(
            "\
Usage: x.py <subcommand> [options] [<paths>...]

Subcommands:
    build       Compile either the compiler or libraries
    check       Compile either the compiler or libraries, using cargo check
    test        Build and run some test suites
    bench       Build and run some benchmarks
    doc         Build documentation
    clean       Clean out build directories
    dist        Build distribution artifacts
    install     Install distribution artifacts

To learn more about a subcommand, run `./x.py <subcommand> -h`"
        );

        let mut opts = Options::new();
        // Options common to all subcommands
        opts.optflagmulti("v", "verbose", "use verbose output (-vv for very verbose)");
        opts.optflag("i", "incremental", "use incremental compilation");
        opts.optopt("", "config", "TOML configuration file for build", "FILE");
        opts.optopt("", "build", "build target of the stage0 compiler", "BUILD");
        opts.optmulti("", "host", "host targets to build", "HOST");
        opts.optmulti("", "target", "target targets to build", "TARGET");
        opts.optmulti("", "exclude", "build paths to exclude", "PATH");
        opts.optopt("", "on-fail", "command to run on failure", "CMD");
        opts.optflag("", "dry-run", "dry run; don't build anything");
        opts.optopt("", "stage",
            "stage to build (indicates compiler to use/test, e.g. stage 0 uses the \
             bootstrap compiler, stage 1 the stage 0 rustc artifacts, etc.)",
            "N");
        opts.optmulti("", "keep-stage", "stage(s) to keep without recompiling \
            (pass multiple times to keep e.g. both stages 0 and 1)", "N");
        opts.optopt("", "src", "path to the root of the rust checkout", "DIR");
        opts.optopt("j", "jobs", "number of jobs to run in parallel", "JOBS");
        opts.optflag("h", "help", "print this help message");
        opts.optopt(
            "",
            "warnings",
            "if value is deny, will deny warnings, otherwise use default",
            "VALUE",
        );
        opts.optopt("", "error-format", "rustc error format", "FORMAT");

        // fn usage()
        let usage =
            |exit_code: i32, opts: &Options, subcommand_help: &str, extra_help: &str| -> ! {
                println!("{}", opts.usage(subcommand_help));
                if !extra_help.is_empty() {
                    println!("{}", extra_help);
                }
                process::exit(exit_code);
            };

        // We can't use getopt to parse the options until we have completed specifying which
        // options are valid, but under the current implementation, some options are conditional on
        // the subcommand. Therefore we must manually identify the subcommand first, so that we can
        // complete the definition of the options.  Then we can use the getopt::Matches object from
        // there on out.
        let subcommand = args.iter().find(|&s| {
            (s == "build")
                || (s == "check")
                || (s == "test")
                || (s == "bench")
                || (s == "doc")
                || (s == "clean")
                || (s == "dist")
                || (s == "install")
        });
        let subcommand = match subcommand {
            Some(s) => s,
            None => {
                // No or an invalid subcommand -- show the general usage and subcommand help
                // An exit code will be 0 when no subcommand is given, and 1 in case of an invalid
                // subcommand.
                println!("{}\n", subcommand_help);
                let exit_code = if args.is_empty() { 0 } else { 1 };
                process::exit(exit_code);
            }
        };

        // Some subcommands get extra options
        match subcommand.as_str() {
            "test" => {
                opts.optflag("", "no-fail-fast", "Run all tests regardless of failure");
                opts.optmulti("", "test-args", "extra arguments", "ARGS");
                opts.optmulti(
                    "",
                    "rustc-args",
                    "extra options to pass the compiler when running tests",
                    "ARGS",
                );
                opts.optflag("", "no-doc", "do not run doc tests");
                opts.optflag("", "doc", "only run doc tests");
                opts.optflag(
                    "",
                    "bless",
                    "update all stderr/stdout files of failing ui tests",
                );
                opts.optopt(
                    "",
                    "compare-mode",
                    "mode describing what file the actual ui output will be compared to",
                    "COMPARE MODE",
                );
            }
            "bench" => {
                opts.optmulti("", "test-args", "extra arguments", "ARGS");
            }
            "clean" => {
                opts.optflag("", "all", "clean all build artifacts");
            }
            _ => {}
        };

        // Done specifying what options are possible, so do the getopts parsing
        let matches = opts.parse(&args[..]).unwrap_or_else(|e| {
            // Invalid argument/option format
            println!("\n{}\n", e);
            usage(1, &opts, &subcommand_help, &extra_help);
        });
        // Extra sanity check to make sure we didn't hit this crazy corner case:
        //
        //     ./x.py --frobulate clean build
        //            ^-- option  ^     ^- actual subcommand
        //                        \_ arg to option could be mistaken as subcommand
        let mut pass_sanity_check = true;
        match matches.free.get(0) {
            Some(check_subcommand) => {
                if check_subcommand != subcommand {
                    pass_sanity_check = false;
                }
            }
            None => {
                pass_sanity_check = false;
            }
        }
        if !pass_sanity_check {
            println!("{}\n", subcommand_help);
            println!(
                "Sorry, I couldn't figure out which subcommand you were trying to specify.\n\
                 You may need to move some options to after the subcommand.\n"
            );
            process::exit(1);
        }
        // Extra help text for some commands
        match subcommand.as_str() {
            "build" => {
                subcommand_help.push_str(
                    "\n
Arguments:
    This subcommand accepts a number of paths to directories to the crates
    and/or artifacts to compile. For example:

        ./x.py build src/libcore
        ./x.py build src/libcore src/libproc_macro
        ./x.py build src/libstd --stage 1

    If no arguments are passed then the complete artifacts for that stage are
    also compiled.

        ./x.py build
        ./x.py build --stage 1

    For a quick build of a usable compiler, you can pass:

        ./x.py build --stage 1 src/libtest

    This will first build everything once (like `--stage 0` without further
    arguments would), and then use the compiler built in stage 0 to build
    src/libtest and its dependencies.
    Once this is done, build/$ARCH/stage1 contains a usable compiler.",
                );
            }
            "check" => {
                subcommand_help.push_str(
                    "\n
Arguments:
    This subcommand accepts a number of paths to directories to the crates
    and/or artifacts to compile. For example:

        ./x.py check src/libcore
        ./x.py check src/libcore src/libproc_macro

    If no arguments are passed then the complete artifacts are compiled: std, test, and rustc. Note
    also that since we use `cargo check`, by default this will automatically enable incremental
    compilation, so there's no need to pass it separately, though it won't hurt. We also completely
    ignore the stage passed, as there's no way to compile in non-stage 0 without actually building
    the compiler.",
                );
            }
            "test" => {
                subcommand_help.push_str(
                    "\n
Arguments:
    This subcommand accepts a number of paths to directories to tests that
    should be compiled and run. For example:

        ./x.py test src/test/run-pass
        ./x.py test src/libstd --test-args hash_map
        ./x.py test src/libstd --stage 0 --no-doc
        ./x.py test src/test/ui --bless
        ./x.py test src/test/ui --compare-mode nll

    Note that `test src/test/* --stage N` does NOT depend on `build src/rustc --stage N`;
    just like `build src/libstd --stage N` it tests the compiler produced by the previous
    stage.

    If no arguments are passed then the complete artifacts for that stage are
    compiled and tested.

        ./x.py test
        ./x.py test --stage 1",
                );
            }
            "doc" => {
                subcommand_help.push_str(
                    "\n
Arguments:
    This subcommand accepts a number of paths to directories of documentation
    to build. For example:

        ./x.py doc src/doc/book
        ./x.py doc src/doc/nomicon
        ./x.py doc src/doc/book src/libstd

    If no arguments are passed then everything is documented:

        ./x.py doc
        ./x.py doc --stage 1",
                );
            }
            _ => {}
        };
        // Get any optional paths which occur after the subcommand
        let paths = matches.free[1..]
            .iter()
            .map(|p| p.into())
            .collect::<Vec<PathBuf>>();

        let cfg_file = matches.opt_str("config").map(PathBuf::from).or_else(|| {
            if fs::metadata("config.toml").is_ok() {
                Some(PathBuf::from("config.toml"))
            } else {
                None
            }
        });

        // All subcommands except `clean` can have an optional "Available paths" section
        if matches.opt_present("verbose") {
            let config = Config::parse(&["build".to_string()]);
            let mut build = Build::new(config);
            metadata::build(&mut build);

            let maybe_rules_help = Builder::get_help(&build, subcommand.as_str());
            extra_help.push_str(maybe_rules_help.unwrap_or_default().as_str());
        } else if subcommand.as_str() != "clean" {
            extra_help.push_str(
                format!(
                    "Run `./x.py {} -h -v` to see a list of available paths.",
                    subcommand
                ).as_str(),
            );
        }

        // User passed in -h/--help?
        if matches.opt_present("help") {
            usage(0, &opts, &subcommand_help, &extra_help);
        }

        let cmd = match subcommand.as_str() {
            "build" => Subcommand::Build { paths: paths },
            "check" => Subcommand::Check { paths: paths },
            "test" => Subcommand::Test {
                paths,
                bless: matches.opt_present("bless"),
                compare_mode: matches.opt_str("compare-mode"),
                test_args: matches.opt_strs("test-args"),
                rustc_args: matches.opt_strs("rustc-args"),
                fail_fast: !matches.opt_present("no-fail-fast"),
                doc_tests: if matches.opt_present("doc") {
                    DocTests::Only
                } else if matches.opt_present("no-doc") {
                    DocTests::No
                } else {
                    DocTests::Yes
                },
            },
            "bench" => Subcommand::Bench {
                paths,
                test_args: matches.opt_strs("test-args"),
            },
            "doc" => Subcommand::Doc { paths: paths },
            "clean" => {
                if paths.len() > 0 {
                    println!("\nclean does not take a path argument\n");
                    usage(1, &opts, &subcommand_help, &extra_help);
                }

                Subcommand::Clean {
                    all: matches.opt_present("all"),
                }
            }
            "dist" => Subcommand::Dist { paths },
            "install" => Subcommand::Install { paths },
            _ => {
                usage(1, &opts, &subcommand_help, &extra_help);
            }
        };

        Flags {
            verbose: matches.opt_count("verbose"),
            stage: matches.opt_str("stage").map(|j| j.parse().unwrap()),
            dry_run: matches.opt_present("dry-run"),
            on_fail: matches.opt_str("on-fail"),
            rustc_error_format: matches.opt_str("error-format"),
            keep_stage: matches.opt_strs("keep-stage")
                .into_iter().map(|j| j.parse().unwrap())
                .collect(),
            host: split(matches.opt_strs("host"))
                .into_iter()
                .map(|x| INTERNER.intern_string(x))
                .collect::<Vec<_>>(),
            target: split(matches.opt_strs("target"))
                .into_iter()
                .map(|x| INTERNER.intern_string(x))
                .collect::<Vec<_>>(),
            config: cfg_file,
            jobs: matches.opt_str("jobs").map(|j| j.parse().unwrap()),
            cmd,
            incremental: matches.opt_present("incremental"),
            exclude: split(matches.opt_strs("exclude"))
                .into_iter()
                .map(|p| p.into())
                .collect::<Vec<_>>(),
            warnings: matches.opt_str("warnings").map(|v| v == "deny"),
        }
    }
}

impl Subcommand {
    pub fn test_args(&self) -> Vec<&str> {
        match *self {
            Subcommand::Test { ref test_args, .. } | Subcommand::Bench { ref test_args, .. } => {
                test_args
                    .iter()
                    .flat_map(|s| s.split_whitespace())
                    .collect()
            }
            _ => Vec::new(),
        }
    }

    pub fn rustc_args(&self) -> Vec<&str> {
        match *self {
            Subcommand::Test { ref rustc_args, .. } => rustc_args
                .iter()
                .flat_map(|s| s.split_whitespace())
                .collect(),
            _ => Vec::new(),
        }
    }

    pub fn fail_fast(&self) -> bool {
        match *self {
            Subcommand::Test { fail_fast, .. } => fail_fast,
            _ => false,
        }
    }

    pub fn doc_tests(&self) -> DocTests {
        match *self {
            Subcommand::Test { doc_tests, .. } => doc_tests,
            _ => DocTests::Yes,
        }
    }

    pub fn bless(&self) -> bool {
        match *self {
            Subcommand::Test { bless, .. } => bless,
            _ => false,
        }
    }

    pub fn compare_mode(&self) -> Option<&str> {
        match *self {
            Subcommand::Test {
                ref compare_mode, ..
            } => compare_mode.as_ref().map(|s| &s[..]),
            _ => None,
        }
    }
}

fn split(s: Vec<String>) -> Vec<String> {
    s.iter()
        .flat_map(|s| s.split(','))
        .map(|s| s.to_string())
        .collect()
}
// Copyright 2016 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of the install aspects of the compiler.
//
// This module is responsible for installing the standard library,
// compiler, and documentation.

use std::env;
use std::fs;
use std::path::{Path, PathBuf, Component};
use std::process::Command;

use dist::{self, pkgname, sanitize_sh, tmpdir};

use builder::{Builder, RunConfig, ShouldRun, Step};
use cache::Interned;
use config::Config;

pub fn install_docs(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "docs", "rust-docs", stage, Some(host));
}

pub fn install_std(builder: &Builder, stage: u32, target: Interned<String>) {
    install_sh(builder, "std", "rust-std", stage, Some(target));
}

pub fn install_cargo(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "cargo", "cargo", stage, Some(host));
}

pub fn install_rls(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "rls", "rls", stage, Some(host));
}
pub fn install_clippy(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "clippy", "clippy", stage, Some(host));
}

pub fn install_rustfmt(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "rustfmt", "rustfmt", stage, Some(host));
}

pub fn install_analysis(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "analysis", "rust-analysis", stage, Some(host));
}

pub fn install_src(builder: &Builder, stage: u32) {
    install_sh(builder, "src", "rust-src", stage, None);
}
pub fn install_rustc(builder: &Builder, stage: u32, host: Interned<String>) {
    install_sh(builder, "rustc", "rustc", stage, Some(host));
}

fn install_sh(
    builder: &Builder,
    package: &str,
    name: &str,
    stage: u32,
    host: Option<Interned<String>>
) {
    builder.info(&format!("Install {} stage{} ({:?})", package, stage, host));

    let prefix_default = PathBuf::from("/usr/local");
    let sysconfdir_default = PathBuf::from("/etc");
    let datadir_default = PathBuf::from("share");
    let docdir_default = datadir_default.join("doc/rust");
    let bindir_default = PathBuf::from("bin");
    let libdir_default = PathBuf::from("lib");
    let mandir_default = datadir_default.join("man");
    let prefix = builder.config.prefix.as_ref().map_or(prefix_default, |p| {
        fs::canonicalize(p).unwrap_or_else(|_| panic!("could not canonicalize {}", p.display()))
    });
    let sysconfdir = builder.config.sysconfdir.as_ref().unwrap_or(&sysconfdir_default);
    let datadir = builder.config.datadir.as_ref().unwrap_or(&datadir_default);
    let docdir = builder.config.docdir.as_ref().unwrap_or(&docdir_default);
    let bindir = builder.config.bindir.as_ref().unwrap_or(&bindir_default);
    let libdir = builder.config.libdir.as_ref().unwrap_or(&libdir_default);
    let mandir = builder.config.mandir.as_ref().unwrap_or(&mandir_default);

    let sysconfdir = prefix.join(sysconfdir);
    let datadir = prefix.join(datadir);
    let docdir = prefix.join(docdir);
    let bindir = prefix.join(bindir);
    let libdir = prefix.join(libdir);
    let mandir = prefix.join(mandir);

    let destdir = env::var_os("DESTDIR").map(PathBuf::from);

    let prefix = add_destdir(&prefix, &destdir);
    let sysconfdir = add_destdir(&sysconfdir, &destdir);
    let datadir = add_destdir(&datadir, &destdir);
    let docdir = add_destdir(&docdir, &destdir);
    let bindir = add_destdir(&bindir, &destdir);
    let libdir = add_destdir(&libdir, &destdir);
    let mandir = add_destdir(&mandir, &destdir);

    let empty_dir = builder.out.join("tmp/empty_dir");

    t!(fs::create_dir_all(&empty_dir));
    let package_name = if let Some(host) = host {
        format!("{}-{}", pkgname(builder, name), host)
    } else {
        pkgname(builder, name)
    };

    let mut cmd = Command::new("sh");
    cmd.current_dir(&empty_dir)
        .arg(sanitize_sh(&tmpdir(builder).join(&package_name).join("install.sh")))
        .arg(format!("--prefix={}", sanitize_sh(&prefix)))
        .arg(format!("--sysconfdir={}", sanitize_sh(&sysconfdir)))
        .arg(format!("--datadir={}", sanitize_sh(&datadir)))
        .arg(format!("--docdir={}", sanitize_sh(&docdir)))
        .arg(format!("--bindir={}", sanitize_sh(&bindir)))
        .arg(format!("--libdir={}", sanitize_sh(&libdir)))
        .arg(format!("--mandir={}", sanitize_sh(&mandir)))
        .arg("--disable-ldconfig");
    builder.run(&mut cmd);
    t!(fs::remove_dir_all(&empty_dir));
}

fn add_destdir(path: &Path, destdir: &Option<PathBuf>) -> PathBuf {
    let mut ret = match *destdir {
        Some(ref dest) => dest.clone(),
        None => return path.to_path_buf(),
    };
    for part in path.components() {
        match part {
            Component::Normal(s) => ret.push(s),
            _ => {}
        }
    }
    ret
}

macro_rules! install {
    (($sel:ident, $builder:ident, $_config:ident),
       $($name:ident,
       $path:expr,
       $default_cond:expr,
       only_hosts: $only_hosts:expr,
       $run_item:block $(, $c:ident)*;)+) => {
        $(
            #[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
        pub struct $name {
            pub stage: u32,
            pub target: Interned<String>,
            pub host: Interned<String>,
        }

        impl $name {
            #[allow(dead_code)]
            fn should_build(config: &Config) -> bool {
                config.extended && config.tools.as_ref()
                    .map_or(true, |t| t.contains($path))
            }

            #[allow(dead_code)]
            fn should_install(builder: &Builder) -> bool {
                builder.config.tools.as_ref().map_or(false, |t| t.contains($path))
            }
        }

        impl Step for $name {
            type Output = ();
            const DEFAULT: bool = true;
            const ONLY_HOSTS: bool = $only_hosts;
            $(const $c: bool = true;)*

            fn should_run(run: ShouldRun) -> ShouldRun {
                let $_config = &run.builder.config;
                run.path($path).default_condition($default_cond)
            }

            fn make_run(run: RunConfig) {
                run.builder.ensure($name {
                    stage: run.builder.top_stage,
                    target: run.target,
                    host: run.builder.config.build,
                });
            }

            fn run($sel, $builder: &Builder) {
                $run_item
            }
        })+
    }
}

install!((self, builder, _config),
    Docs, "src/doc", _config.docs, only_hosts: false, {
        builder.ensure(dist::Docs { stage: self.stage, host: self.target });
        install_docs(builder, self.stage, self.target);
    };
    Std, "src/libstd", true, only_hosts: true, {
        for target in &builder.targets {
            builder.ensure(dist::Std {
                compiler: builder.compiler(self.stage, self.host),
                target: *target
            });
            install_std(builder, self.stage, *target);
        }
    };
    Cargo, "cargo", Self::should_build(_config), only_hosts: true, {
        builder.ensure(dist::Cargo { stage: self.stage, target: self.target });
        install_cargo(builder, self.stage, self.target);
    };
    Rls, "rls", Self::should_build(_config), only_hosts: true, {
        if builder.ensure(dist::Rls { stage: self.stage, target: self.target }).is_some() ||
            Self::should_install(builder) {
            install_rls(builder, self.stage, self.target);
        } else {
            builder.info(&format!("skipping Install RLS stage{} ({})", self.stage, self.target));
        }
    };
    Clippy, "clippy", Self::should_build(_config), only_hosts: true, {
        if builder.ensure(dist::Clippy { stage: self.stage, target: self.target }).is_some() ||
            Self::should_install(builder) {
            install_clippy(builder, self.stage, self.target);
        } else {
            builder.info(&format!("skipping Install clippy stage{} ({})", self.stage, self.target));
        }
    };
    Rustfmt, "rustfmt", Self::should_build(_config), only_hosts: true, {
        if builder.ensure(dist::Rustfmt { stage: self.stage, target: self.target }).is_some() ||
            Self::should_install(builder) {
            install_rustfmt(builder, self.stage, self.target);
        } else {
            builder.info(
                &format!("skipping Install Rustfmt stage{} ({})", self.stage, self.target));
        }
    };
    Analysis, "analysis", Self::should_build(_config), only_hosts: false, {
        builder.ensure(dist::Analysis {
            compiler: builder.compiler(self.stage, self.host),
            target: self.target
        });
        install_analysis(builder, self.stage, self.target);
    };
    Rustc, "src/librustc", true, only_hosts: true, {
        builder.ensure(dist::Rustc {
            compiler: builder.compiler(self.stage, self.target),
        });
        install_rustc(builder, self.stage, self.target);
    };
);

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Src {
    pub stage: u32,
}

impl Step for Src {
    type Output = ();
    const DEFAULT: bool = true;
    const ONLY_HOSTS: bool = true;

    fn should_run(run: ShouldRun) -> ShouldRun {
        let config = &run.builder.config;
        let cond = config.extended &&
            config.tools.as_ref().map_or(true, |t| t.contains("src"));
        run.path("src").default_condition(cond)
    }

    fn make_run(run: RunConfig) {
        run.builder.ensure(Src {
            stage: run.builder.top_stage,
        });
    }

    fn run(self, builder: &Builder) {
        builder.ensure(dist::Src);
        install_src(builder, self.stage);
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Job management on Windows for bootstrapping
//
// Most of the time when you're running a build system (e.g. make) you expect
// Ctrl-C or abnormal termination to actually terminate the entire tree of
// process in play, not just the one at the top. This currently works "by
// default" on Unix platforms because Ctrl-C actually sends a signal to the
// *process group* rather than the parent process, so everything will get torn
// down. On Windows, however, this does not happen and Ctrl-C just kills the
// parent process.
//
// To achieve the same semantics on Windows we use Job Objects to ensure that
// all processes die at the same time. Job objects have a mode of operation
// where when all handles to the object are closed it causes all child
// processes associated with the object to be terminated immediately.
// Conveniently whenever a process in the job object spawns a new process the
// child will be associated with the job object as well. This means if we add
// ourselves to the job object we create then everything will get torn down!
//
// Unfortunately most of the time the build system is actually called from a
// python wrapper (which manages things like building the build system) so this
// all doesn't quite cut it so far. To go the last mile we duplicate the job
// object handle into our parent process (a python process probably) and then
// close our own handle. This means that the only handle to the job object
// resides in the parent python process, so when python dies the whole build
// system dies (as one would probably expect!).
//
// Note that this module has a #[cfg(windows)] above it as none of this logic
// is required on Unix.

#[allow(nonstandard_style, dead_code)]

use std::env;
use std::io;
use std::mem;
use Build;

type HANDLE = *mut u8;
type BOOL = i32;
type DWORD = u32;
type LPHANDLE = *mut HANDLE;
type LPVOID = *mut u8;
type JOBOBJECTINFOCLASS = i32;
type SIZE_T = usize;
type LARGE_INTEGER = i64;
type UINT = u32;
type ULONG_PTR = usize;
type ULONGLONG = u64;

const FALSE: BOOL = 0;
const DUPLICATE_SAME_ACCESS: DWORD = 0x2;
const PROCESS_DUP_HANDLE: DWORD = 0x40;
const JobObjectExtendedLimitInformation: JOBOBJECTINFOCLASS = 9;
const JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE: DWORD = 0x2000;
const JOB_OBJECT_LIMIT_PRIORITY_CLASS: DWORD = 0x00000020;
const SEM_FAILCRITICALERRORS: UINT = 0x0001;
const SEM_NOGPFAULTERRORBOX: UINT = 0x0002;
const BELOW_NORMAL_PRIORITY_CLASS: DWORD = 0x00004000;

extern "system" {
    fn CreateJobObjectW(lpJobAttributes: *mut u8, lpName: *const u8) -> HANDLE;
    fn CloseHandle(hObject: HANDLE) -> BOOL;
    fn GetCurrentProcess() -> HANDLE;
    fn OpenProcess(dwDesiredAccess: DWORD,
                   bInheritHandle: BOOL,
                   dwProcessId: DWORD) -> HANDLE;
    fn DuplicateHandle(hSourceProcessHandle: HANDLE,
                       hSourceHandle: HANDLE,
                       hTargetProcessHandle: HANDLE,
                       lpTargetHandle: LPHANDLE,
                       dwDesiredAccess: DWORD,
                       bInheritHandle: BOOL,
                       dwOptions: DWORD) -> BOOL;
    fn AssignProcessToJobObject(hJob: HANDLE, hProcess: HANDLE) -> BOOL;
    fn SetInformationJobObject(hJob: HANDLE,
                               JobObjectInformationClass: JOBOBJECTINFOCLASS,
                               lpJobObjectInformation: LPVOID,
                               cbJobObjectInformationLength: DWORD) -> BOOL;
    fn SetErrorMode(mode: UINT) -> UINT;
}

#[repr(C)]
struct JOBOBJECT_EXTENDED_LIMIT_INFORMATION {
    BasicLimitInformation: JOBOBJECT_BASIC_LIMIT_INFORMATION,
    IoInfo: IO_COUNTERS,
    ProcessMemoryLimit: SIZE_T,
    JobMemoryLimit: SIZE_T,
    PeakProcessMemoryUsed: SIZE_T,
    PeakJobMemoryUsed: SIZE_T,
}

#[repr(C)]
struct IO_COUNTERS {
    ReadOperationCount: ULONGLONG,
    WriteOperationCount: ULONGLONG,
    OtherOperationCount: ULONGLONG,
    ReadTransferCount: ULONGLONG,
    WriteTransferCount: ULONGLONG,
    OtherTransferCount: ULONGLONG,
}

#[repr(C)]
struct JOBOBJECT_BASIC_LIMIT_INFORMATION {
    PerProcessUserTimeLimit: LARGE_INTEGER,
    PerJobUserTimeLimit: LARGE_INTEGER,
    LimitFlags: DWORD,
    MinimumWorkingsetSize: SIZE_T,
    MaximumWorkingsetSize: SIZE_T,
    ActiveProcessLimit: DWORD,
    Affinity: ULONG_PTR,
    PriorityClass: DWORD,
    SchedulingClass: DWORD,
}

pub unsafe fn setup(build: &mut Build) {
    // Enable the Windows Error Reporting dialog which msys disables,
    // so we can JIT debug rustc
    let mode = SetErrorMode(0);
    SetErrorMode(mode & !SEM_NOGPFAULTERRORBOX);

    // Create a new job object for us to use
    let job = CreateJobObjectW(0 as *mut _, 0 as *const _);
    assert!(job != 0 as *mut _, "{}", io::Error::last_os_error());

    // Indicate that when all handles to the job object are gone that all
    // process in the object should be killed. Note that this includes our
    // entire process tree by default because we've added ourselves and our
    // children will reside in the job by default.
    let mut info = mem::zeroed::<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>();
    info.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;
    if build.config.low_priority {
        info.BasicLimitInformation.LimitFlags |= JOB_OBJECT_LIMIT_PRIORITY_CLASS;
        info.BasicLimitInformation.PriorityClass = BELOW_NORMAL_PRIORITY_CLASS;
    }
    let r = SetInformationJobObject(job,
                                    JobObjectExtendedLimitInformation,
                                    &mut info as *mut _ as LPVOID,
                                    mem::size_of_val(&info) as DWORD);
    assert!(r != 0, "{}", io::Error::last_os_error());

    // Assign our process to this job object. Note that if this fails, one very
    // likely reason is that we are ourselves already in a job object! This can
    // happen on the build bots that we've got for Windows, or if just anyone
    // else is instrumenting the build. In this case we just bail out
    // immediately and assume that they take care of it.
    //
    // Also note that nested jobs (why this might fail) are supported in recent
    // versions of Windows, but the version of Windows that our bots are running
    // at least don't support nested job objects.
    let r = AssignProcessToJobObject(job, GetCurrentProcess());
    if r == 0 {
        CloseHandle(job);
        return
    }

    // If we've got a parent process (e.g. the python script that called us)
    // then move ownership of this job object up to them. That way if the python
    // script is killed (e.g. via ctrl-c) then we'll all be torn down.
    //
    // If we don't have a parent (e.g. this was run directly) then we
    // intentionally leak the job object handle. When our process exits
    // (normally or abnormally) it will close the handle implicitly, causing all
    // processes in the job to be cleaned up.
    let pid = match env::var("BOOTSTRAP_PARENT_ID") {
        Ok(s) => s,
        Err(..) => return,
    };

    let parent = OpenProcess(PROCESS_DUP_HANDLE, FALSE, pid.parse().unwrap());
    assert!(parent != 0 as *mut _, "{}", io::Error::last_os_error());
    let mut parent_handle = 0 as *mut _;
    let r = DuplicateHandle(GetCurrentProcess(), job,
                            parent, &mut parent_handle,
                            0, FALSE, DUPLICATE_SAME_ACCESS);

    // If this failed, well at least we tried! An example of DuplicateHandle
    // failing in the past has been when the wrong python2 package spawned this
    // build system (e.g. the `python2` package in MSYS instead of
    // `mingw-w64-x86_64-python2`. Not sure why it failed, but the "failure
    // mode" here is that we only clean everything up when the build system
    // dies, not when the python parent does, so not too bad.
    if r != 0 {
        CloseHandle(job);
    }
}
// Copyright 2015 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// Implementation of rustbuild, the Rust build system.
//
// This module, and its descendants, are the implementation of the Rust build
// system. Most of this build system is backed by Cargo but the outer layer
// here serves as the ability to orchestrate calling Cargo, sequencing Cargo
// builds, building artifacts like LLVM, etc. The goals of rustbuild are:
//
// * To be an easily understandable, easily extensible, and maintainable build
//   system.
// * Leverage standard tools in the Rust ecosystem to build the compiler, aka
//   crates.io and Cargo.
// * A standard interface to build across all platforms, including MSVC
//
// ## Architecture
//
// The build system defers most of the complicated logic managing invocations
// of rustc and rustdoc to Cargo itself. However, moving through various stages
// and copying artifacts is still necessary for it to do. Each time rustbuild
// is invoked, it will iterate through the list of predefined steps and execute
// each serially in turn if it matches the paths passed or is a default rule.
// For each step rustbuild relies on the step internally being incremental and
// parallel. Note, though, that the `-j` parameter to rustbuild gets forwarded
// to appropriate test harnesses and such.
//
// Most of the "meaty" steps that matter are backed by Cargo, which does indeed
// have its own parallelism and incremental management. Later steps, like
// tests, aren't incremental and simply run the entire suite currently.
// However, compiletest itself tries to avoid running tests when the artifacts
// that are involved (mainly the compiler) haven't changed.
//
// When you execute `x.py build`, the steps which are executed are:
//
// * First, the python script is run. This will automatically download the
//   stage0 rustc and cargo according to `src/stage0.txt`, or use the cached
//   versions if they're available. These are then used to compile rustbuild
//   itself (using Cargo). Finally, control is then transferred to rustbuild.
//
// * Rustbuild takes over, performs sanity checks, probes the environment,
//   reads configuration, and starts executing steps as it reads the command
//   line arguments (paths) or going through the default rules.
//
//   The build output will be something like the following:
//
//   Building stage0 std artifacts
//   Copying stage0 std
//   Building stage0 test artifacts
//   Copying stage0 test
//   Building stage0 compiler artifacts
//   Copying stage0 rustc
//   Assembling stage1 compiler
//   Building stage1 std artifacts
//   Copying stage1 std
//   Building stage1 test artifacts
//   Copying stage1 test
//   Building stage1 compiler artifacts
//   Copying stage1 rustc
//   Assembling stage2 compiler
//   Uplifting stage1 std
//   Uplifting stage1 test
//   Uplifting stage1 rustc
//
// Let's disect that a little:
//
// ## Building stage0 {std,test,compiler} artifacts
//
// These steps use the provided (downloaded, usually) compiler to compile the
// local Rust source into libraries we can use.
//
// ## Copying stage0 {std,test,rustc}
//
// This copies the build output from Cargo into
// `build/$HOST/stage0-sysroot/lib/rustlib/$ARCH/lib`. FIXME: This step's
// documentation should be expanded -- the information already here may be
// incorrect.
//
// ## Assembling stage1 compiler
//
// This copies the libraries we built in "building stage0 ... artifacts" into
// the stage1 compiler's lib directory. These are the host libraries that the
// compiler itself uses to run. These aren't actually used by artifacts the new
// compiler generates. This step also copies the rustc and rustdoc binaries we
// generated into build/$HOST/stage/bin.
//
// The stage1/bin/rustc is a fully functional compiler, but it doesn't yet have
// any libraries to link built binaries or libraries to. The next 3 steps will
// provide those libraries for it; they are mostly equivalent to constructing
// the stage1/bin compiler so we don't go through them individually.
//
// ## Uplifting stage1 {std,test,rustc}
//
// This step copies the libraries from the stage1 compiler sysroot into the
// stage2 compiler. This is done to avoid rebuilding the compiler; libraries
// we'd build in this step should be identical (in function, if not necessarily
// identical on disk) so there's no need to recompile the compiler again. Note
// that if you want to, you can enable the full-bootstrap option to change this
// behavior.
//
// Each step is driven by a separate Cargo project and rustbuild orchestrates
// copying files between steps and otherwise preparing for Cargo to run.
//
// ## Further information
//
// More documentation can be found in each respective module below, and you can
// also check out the `src/bootstrap/README.md` file for more information.

#[deny(bare_trait_objects)]
#[deny(warnings)]
#[feature(core_intrinsics)]
#[feature(drain_filter)]

#[macro_use]
extern crate build_helper;
#[macro_use]
extern crate serde_derive;
#[macro_use]
extern crate lazy_static;
extern crate serde_json;
extern crate cmake;
extern crate filetime;
extern crate cc;
extern crate getopts;
extern crate num_cpus;
extern crate toml;
extern crate time;
extern crate petgraph;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[cfg(unix)]
extern crate libc;

use std::cell::{RefCell, Cell};
use std::collections::{HashSet, HashMap};
use std::env;
use std::fs::{self, OpenOptions, File};
use std::io::{self, Seek, SeekFrom, Write, Read};
use std::path::{PathBuf, Path};
use std::process::{self, Command};
use std::slice;
use std::str;

#[cfg(unix)]
use std::os::unix::fs::symlink as symlink_file;
#[cfg(windows)]
use std::os::windows::fs::symlink_file;

use build_helper::{run_silent, run_suppressed, try_run_silent, try_run_suppressed, output, mtime};
use filetime::FileTime;

use util::{exe, libdir, OutputFolder, CiEnv};

mod cc_detect;
mod channel;
mod check;
mod test;
mod clean;
mod compile;
mod metadata;
mod config;
mod dist;
mod doc;
mod flags;
mod install;
mod native;
mod sanity;
pub mod util;
mod builder;
mod cache;
mod tool;
mod toolstate;

#[cfg(windows)]
mod job;

#[cfg(all(unix, not(target_os = "haiku")))]
mod job {
    use libc;

    pub unsafe fn setup(build: &mut ::Build) {
        if build.config.low_priority {
            libc::setpriority(libc::PRIO_PGRP as _, 0, 10);
        }
    }
}

#[cfg(any(target_os = "haiku", not(any(unix, windows))))]
mod job {
    pub unsafe fn setup(_build: &mut ::Build) {
    }
}

pub use config::Config;
use flags::Subcommand;
use cache::{Interned, INTERNER};
use toolstate::ToolState;

const LLVM_TOOLS: &[&str] = &[
    "llvm-nm", // used to inspect binaries; it shows symbol names, their sizes and visibility
    "llvm-objcopy", // used to transform ELFs into binary format which flashing tools consume
    "llvm-objdump", // used to disassemble programs
    "llvm-profdata", // used to inspect and merge files generated by profiles
    "llvm-readobj", // used to get information from ELFs/objects that the other tools don't provide
    "llvm-size", // used to prints the size of the linker sections of a program
    "llvm-strip", // used to discard symbols from binary files to reduce their size
];

/// A structure representing a Rust compiler.
///
/// Each compiler has a `stage` that it is associated with and a `host` that
/// corresponds to the platform the compiler runs on. This structure is used as
/// a parameter to many methods below.
#[derive(Eq, PartialOrd, Ord, PartialEq, Clone, Copy, Hash, Debug)]
pub struct Compiler {
    stage: u32,
    host: Interned<String>,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum DocTests {
    // Default, run normal tests and doc tests.
    Yes,
    // Do not run any doc tests.
    No,
    // Only run doc tests.
    Only,
}

/// Global configuration for the build system.
///
/// This structure transitively contains all configuration for the build system.
/// All filesystem-encoded configuration is in `config`, all flags are in
/// `flags`, and then parsed or probed information is listed in the keys below.
///
/// This structure is a parameter of almost all methods in the build system,
/// although most functions are implemented as free functions rather than
/// methods specifically on this structure itself (to make it easier to
/// organize).
pub struct Build {
    // User-specified configuration via config.toml
    config: Config,

    // Derived properties from the above two configurations
    src: PathBuf,
    out: PathBuf,
    rust_info: channel::GitInfo,
    cargo_info: channel::GitInfo,
    rls_info: channel::GitInfo,
    clippy_info: channel::GitInfo,
    rustfmt_info: channel::GitInfo,
    local_rebuild: bool,
    fail_fast: bool,
    doc_tests: DocTests,
    verbosity: usize,

    // Targets for which to build.
    build: Interned<String>,
    hosts: Vec<Interned<String>>,
    targets: Vec<Interned<String>>,

    // Stage 0 (downloaded) compiler and cargo or their local rust equivalents.
    initial_rustc: PathBuf,
    initial_cargo: PathBuf,

    // Probed tools at runtime
    lldb_version: Option<String>,
    lldb_python_dir: Option<String>,

    // Runtime state filled in later on
    // C/C++ compilers and archiver for all targets
    cc: HashMap<Interned<String>, cc::Tool>,
    cxx: HashMap<Interned<String>, cc::Tool>,
    ar: HashMap<Interned<String>, PathBuf>,
    ranlib: HashMap<Interned<String>, PathBuf>,
    // Misc
    crates: HashMap<Interned<String>, Crate>,
    is_sudo: bool,
    ci_env: CiEnv,
    delayed_failures: RefCell<Vec<String>>,
    prerelease_version: Cell<Option<u32>>,
    tool_artifacts: RefCell<HashMap<
        Interned<String>,
        HashMap<String, (&'static str, PathBuf, Vec<String>)>
    >>,
}

#[derive(Debug)]
struct Crate {
    name: Interned<String>,
    version: String,
    deps: HashSet<Interned<String>>,
    id: String,
    path: PathBuf,
    doc_step: String,
    build_step: String,
    test_step: String,
    bench_step: String,
}

impl Crate {
    fn is_local(&self, build: &Build) -> bool {
        self.path.starts_with(&build.config.src) &&
        !self.path.to_string_lossy().ends_with("_shim")
    }

    fn local_path(&self, build: &Build) -> PathBuf {
        assert!(self.is_local(build));
        self.path.strip_prefix(&build.config.src).unwrap().into()
    }
}

/// The various "modes" of invoking Cargo.
///
/// These entries currently correspond to the various output directories of the
/// build system, with each mod generating output in a different directory.
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mode {
    /// Build the standard library, placing output in the "stageN-std" directory.
    Std,

    /// Build libtest, placing output in the "stageN-test" directory.
    Test,

    /// Build librustc, and compiler libraries, placing output in the "stageN-rustc" directory.
    Rustc,

    /// Build codegen libraries, placing output in the "stageN-codegen" directory
    Codegen,

    /// Build some tools, placing output in the "stageN-tools" directory. The
    /// "other" here is for miscellaneous sets of tools that are built using the
    /// bootstrap compiler in its entirety (target libraries and all).
    /// Typically these tools compile with stable Rust.
    ToolBootstrap,

    /// Compile a tool which uses all libraries we compile (up to rustc).
    /// Doesn't use the stage0 compiler libraries like "other", and includes
    /// tools like rustdoc, cargo, rls, etc.
    ToolStd,
    ToolRustc,
}

impl Mode {
    pub fn is_tool(&self) -> bool {
        match self {
            Mode::ToolBootstrap | Mode::ToolRustc | Mode::ToolStd => true,
            _ => false
        }
    }
}

impl Build {
    /// Creates a new set of build configuration from the `flags` on the command
    /// line and the filesystem `config`.
    ///
    /// By default all build output will be placed in the current directory.
    pub fn new(config: Config) -> Build {
        let src = config.src.clone();
        let out = config.out.clone();

        let is_sudo = match env::var_os("SUDO_USER") {
            Some(sudo_user) => {
                match env::var_os("USER") {
                    Some(user) => user != sudo_user,
                    None => false,
                }
            }
            None => false,
        };
        let rust_info = channel::GitInfo::new(&config, &src);
        let cargo_info = channel::GitInfo::new(&config, &src.join("src/tools/cargo"));
        let rls_info = channel::GitInfo::new(&config, &src.join("src/tools/rls"));
        let clippy_info = channel::GitInfo::new(&config, &src.join("src/tools/clippy"));
        let rustfmt_info = channel::GitInfo::new(&config, &src.join("src/tools/rustfmt"));

        let mut build = Build {
            initial_rustc: config.initial_rustc.clone(),
            initial_cargo: config.initial_cargo.clone(),
            local_rebuild: config.local_rebuild,
            fail_fast: config.cmd.fail_fast(),
            doc_tests: config.cmd.doc_tests(),
            verbosity: config.verbose,

            build: config.build,
            hosts: config.hosts.clone(),
            targets: config.targets.clone(),

            config,
            src,
            out,

            rust_info,
            cargo_info,
            rls_info,
            clippy_info,
            rustfmt_info,
            cc: HashMap::new(),
            cxx: HashMap::new(),
            ar: HashMap::new(),
            ranlib: HashMap::new(),
            crates: HashMap::new(),
            lldb_version: None,
            lldb_python_dir: None,
            is_sudo,
            ci_env: CiEnv::current(),
            delayed_failures: RefCell::new(Vec::new()),
            prerelease_version: Cell::new(None),
            tool_artifacts: Default::default(),
        };

        build.verbose("finding compilers");
        cc_detect::find(&mut build);
        build.verbose("running sanity check");
        sanity::check(&mut build);

        // If local-rust is the same major.minor as the current version, then force a
        // local-rebuild
        let local_version_verbose = output(
            Command::new(&build.initial_rustc).arg("--version").arg("--verbose"));
        let local_release = local_version_verbose
            .lines().filter(|x| x.starts_with("release:"))
            .next().unwrap().trim_left_matches("release:").trim();
        let my_version = channel::CFG_RELEASE_NUM;
        if local_release.split('.').take(2).eq(my_version.split('.').take(2)) {
            build.verbose(&format!("auto-detected local-rebuild {}", local_release));
            build.local_rebuild = true;
        }

        build.verbose("learning about cargo");
        metadata::build(&mut build);

        build
    }

    pub fn build_triple(&self) -> &[Interned<String>] {
        unsafe {
            slice::from_raw_parts(&self.build, 1)
        }
    }

    /// Executes the entire build, as configured by the flags and configuration.
    pub fn build(&mut self) {
        unsafe {
            job::setup(self);
        }

        if let Subcommand::Clean { all } = self.config.cmd {
            return clean::clean(self, all);
        }

        {
            let builder = builder::Builder::new(&self);
            if let Some(path) = builder.paths.get(0) {
                if path == Path::new("nonexistent/path/to/trigger/cargo/metadata") {
                    return;
                }
            }
        }

        if !self.config.dry_run {
            {
                self.config.dry_run = true;
                let builder = builder::Builder::new(&self);
                builder.execute_cli();
            }
            self.config.dry_run = false;
            let builder = builder::Builder::new(&self);
            builder.execute_cli();
        } else {
            let builder = builder::Builder::new(&self);
            let _ = builder.execute_cli();
        }

        // Check for postponed failures from `test --no-fail-fast`.
        let failures = self.delayed_failures.borrow();
        if failures.len() > 0 {
            println!("\n{} command(s) did not execute successfully:\n", failures.len());
            for failure in failures.iter() {
                println!("  - {}\n", failure);
            }
            process::exit(1);
        }
    }

    /// Clear out `dir` if `input` is newer.
    ///
    /// After this executes, it will also ensure that `dir` exists.
    fn clear_if_dirty(&self, dir: &Path, input: &Path) -> bool {
        let stamp = dir.join(".stamp");
        let mut cleared = false;
        if mtime(&stamp) < mtime(input) {
            self.verbose(&format!("Dirty - {}", dir.display()));
            let _ = fs::remove_dir_all(dir);
            cleared = true;
        } else if stamp.exists() {
            return cleared;
        }
        t!(fs::create_dir_all(dir));
        t!(File::create(stamp));
        cleared
    }

    /// Get the space-separated set of activated features for the standard
    /// library.
    fn std_features(&self) -> String {
        let mut features = "panic-unwind".to_string();

        if self.config.debug_jemalloc {
            features.push_str(" debug-jemalloc");
        }
        if self.config.use_jemalloc {
            features.push_str(" jemalloc");
        }
        if self.config.backtrace {
            features.push_str(" backtrace");
        }
        if self.config.profiler {
            features.push_str(" profiler");
        }
        if self.config.wasm_syscall {
            features.push_str(" wasm_syscall");
        }
        features
    }

    /// Get the space-separated set of activated features for the compiler.
    fn rustc_features(&self) -> String {
        let mut features = String::new();
        if self.config.use_jemalloc {
            features.push_str(" jemalloc");
        }
        features
    }

    /// Component directory that Cargo will produce output into (e.g.
    /// release/debug)
    fn cargo_dir(&self) -> &'static str {
        if self.config.rust_optimize {"release"} else {"debug"}
    }

    fn tools_dir(&self, compiler: Compiler) -> PathBuf {
        let out = self.out.join(&*compiler.host).join(format!("stage{}-tools-bin", compiler.stage));
        t!(fs::create_dir_all(&out));
        out
    }

    /// Returns the root directory for all output generated in a particular
    /// stage when running with a particular host compiler.
    ///
    /// The mode indicates what the root directory is for.
    fn stage_out(&self, compiler: Compiler, mode: Mode) -> PathBuf {
        let suffix = match mode {
            Mode::Std => "-std",
            Mode::Test => "-test",
            Mode::Rustc => "-rustc",
            Mode::Codegen => "-codegen",
            Mode::ToolBootstrap => "-bootstrap-tools",
            Mode::ToolStd => "-tools",
            Mode::ToolRustc => "-tools",
        };
        self.out.join(&*compiler.host)
                .join(format!("stage{}{}", compiler.stage, suffix))
    }

    /// Returns the root output directory for all Cargo output in a given stage,
    /// running a particular compiler, whether or not we're building the
    /// standard library, and targeting the specified architecture.
    fn cargo_out(&self,
                 compiler: Compiler,
                 mode: Mode,
                 target: Interned<String>) -> PathBuf {
        self.stage_out(compiler, mode).join(&*target).join(self.cargo_dir())
    }

    /// Root output directory for LLVM compiled for `target`
    ///
    /// Note that if LLVM is configured externally then the directory returned
    /// will likely be empty.
    fn llvm_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("llvm")
    }

    fn emscripten_llvm_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("llvm-emscripten")
    }

    fn lld_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("lld")
    }

    /// Output directory for all documentation for a target
    fn doc_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("doc")
    }

    /// Output directory for all documentation for a target
    fn compiler_doc_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("compiler-doc")
    }

    /// Output directory for some generated md crate documentation for a target (temporary)
    fn md_doc_out(&self, target: Interned<String>) -> Interned<PathBuf> {
        INTERNER.intern_path(self.out.join(&*target).join("md-doc"))
    }

    /// Output directory for all crate documentation for a target (temporary)
    ///
    /// The artifacts here are then copied into `doc_out` above.
    fn crate_doc_out(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("crate-docs")
    }

    /// Returns true if no custom `llvm-config` is set for the specified target.
    ///
    /// If no custom `llvm-config` was specified then Rust's llvm will be used.
    fn is_rust_llvm(&self, target: Interned<String>) -> bool {
        match self.config.target_config.get(&target) {
            Some(ref c) => c.llvm_config.is_none(),
            None => true
        }
    }

    /// Returns the path to `FileCheck` binary for the specified target
    fn llvm_filecheck(&self, target: Interned<String>) -> PathBuf {
        let target_config = self.config.target_config.get(&target);
        if let Some(s) = target_config.and_then(|c| c.llvm_config.as_ref()) {
            let llvm_bindir = output(Command::new(s).arg("--bindir"));
            Path::new(llvm_bindir.trim()).join(exe("FileCheck", &*target))
        } else {
            let base = self.llvm_out(self.config.build).join("build");
            let base = if !self.config.ninja && self.config.build.contains("msvc") {
                if self.config.llvm_optimize {
                    if self.config.llvm_release_debuginfo {
                        base.join("RelWithDebInfo")
                    } else {
                        base.join("Release")
                    }
                } else {
                    base.join("Debug")
                }
            } else {
                base
            };
            base.join("bin").join(exe("FileCheck", &*target))
        }
    }

    /// Directory for libraries built from C/C++ code and shared between stages.
    fn native_dir(&self, target: Interned<String>) -> PathBuf {
        self.out.join(&*target).join("native")
    }

    /// Root output directory for rust_test_helpers library compiled for
    /// `target`
    fn test_helpers_out(&self, target: Interned<String>) -> PathBuf {
        self.native_dir(target).join("rust-test-helpers")
    }

    /// Adds the `RUST_TEST_THREADS` env var if necessary
    fn add_rust_test_threads(&self, cmd: &mut Command) {
        if env::var_os("RUST_TEST_THREADS").is_none() {
            cmd.env("RUST_TEST_THREADS", self.jobs().to_string());
        }
    }

    /// Returns the libdir of the snapshot compiler.
    fn rustc_snapshot_libdir(&self) -> PathBuf {
        self.rustc_snapshot_sysroot().join(libdir(&self.config.build))
    }

    /// Returns the sysroot of the snapshot compiler.
    fn rustc_snapshot_sysroot(&self) -> &Path {
        self.initial_rustc.parent().unwrap().parent().unwrap()
    }

    /// Runs a command, printing out nice contextual information if it fails.
    fn run(&self, cmd: &mut Command) {
        if self.config.dry_run { return; }
        self.verbose(&format!("running: {:?}", cmd));
        run_silent(cmd)
    }

    /// Runs a command, printing out nice contextual information if it fails.
    fn run_quiet(&self, cmd: &mut Command) {
        if self.config.dry_run { return; }
        self.verbose(&format!("running: {:?}", cmd));
        run_suppressed(cmd)
    }

    /// Runs a command, printing out nice contextual information if it fails.
    /// Exits if the command failed to execute at all, otherwise returns its
    /// `status.success()`.
    fn try_run(&self, cmd: &mut Command) -> bool {
        if self.config.dry_run { return true; }
        self.verbose(&format!("running: {:?}", cmd));
        try_run_silent(cmd)
    }

    /// Runs a command, printing out nice contextual information if it fails.
    /// Exits if the command failed to execute at all, otherwise returns its
    /// `status.success()`.
    fn try_run_quiet(&self, cmd: &mut Command) -> bool {
        if self.config.dry_run { return true; }
        self.verbose(&format!("running: {:?}", cmd));
        try_run_suppressed(cmd)
    }

    pub fn is_verbose(&self) -> bool {
        self.verbosity > 0
    }

    /// Prints a message if this build is configured in verbose mode.
    fn verbose(&self, msg: &str) {
        if self.is_verbose() {
            println!("{}", msg);
        }
    }

    fn info(&self, msg: &str) {
        if self.config.dry_run { return; }
        println!("{}", msg);
    }

    /// Returns the number of parallel jobs that have been configured for this
    /// build.
    fn jobs(&self) -> u32 {
        self.config.jobs.unwrap_or_else(|| num_cpus::get() as u32)
    }

    /// Returns the path to the C compiler for the target specified.
    fn cc(&self, target: Interned<String>) -> &Path {
        self.cc[&target].path()
    }

    /// Returns a list of flags to pass to the C compiler for the target
    /// specified.
    fn cflags(&self, target: Interned<String>) -> Vec<String> {
        // Filter out -O and /O (the optimization flags) that we picked up from
        // cc-rs because the build scripts will determine that for themselves.
        let mut base = self.cc[&target].args().iter()
                           .map(|s| s.to_string_lossy().into_owned())
                           .filter(|s| !s.starts_with("-O") && !s.starts_with("/O"))
                           .collect::<Vec<_>>();

        // If we're compiling on macOS then we add a few unconditional flags
        // indicating that we want libc++ (more filled out than libstdc++) and
        // we want to compile for 10.7. This way we can ensure that
        // LLVM/jemalloc/etc are all properly compiled.
        if target.contains("apple-darwin") {
            base.push("-stdlib=libc++".into());
        }

        // Work around an apparently bad MinGW / GCC optimization,
        // See: http://lists.llvm.org/pipermail/cfe-dev/2016-December/051980.html
        // See: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=78936
        if &*target == "i686-pc-windows-gnu" {
            base.push("-fno-omit-frame-pointer".into());
        }
        base
    }

    /// Returns the path to the `ar` archive utility for the target specified.
    fn ar(&self, target: Interned<String>) -> Option<&Path> {
        self.ar.get(&target).map(|p| &**p)
    }

    /// Returns the path to the `ranlib` utility for the target specified.
    fn ranlib(&self, target: Interned<String>) -> Option<&Path> {
        self.ranlib.get(&target).map(|p| &**p)
    }

    /// Returns the path to the C++ compiler for the target specified.
    fn cxx(&self, target: Interned<String>) -> Result<&Path, String> {
        match self.cxx.get(&target) {
            Some(p) => Ok(p.path()),
            None => Err(format!(
                    "target `{}` is not configured as a host, only as a target",
                    target))
        }
    }

    /// Returns the path to the linker for the given target if it needs to be overridden.
    fn linker(&self, target: Interned<String>) -> Option<&Path> {
        if let Some(linker) = self.config.target_config.get(&target)
                                                       .and_then(|c| c.linker.as_ref()) {
            Some(linker)
        } else if target != self.config.build &&
                  !target.contains("msvc") &&
                  !target.contains("emscripten") &&
                  !target.contains("wasm32") {
            Some(self.cc(target))
        } else {
            None
        }
    }

    /// Returns if this target should statically link the C runtime, if specified
    fn crt_static(&self, target: Interned<String>) -> Option<bool> {
        if target.contains("pc-windows-msvc") {
            Some(true)
        } else {
            self.config.target_config.get(&target)
                .and_then(|t| t.crt_static)
        }
    }

    /// Returns the "musl root" for this `target`, if defined
    fn musl_root(&self, target: Interned<String>) -> Option<&Path> {
        self.config.target_config.get(&target)
            .and_then(|t| t.musl_root.as_ref())
            .or(self.config.musl_root.as_ref())
            .map(|p| &**p)
    }

    /// Returns true if this is a no-std `target`, if defined
    fn no_std(&self, target: Interned<String>) -> Option<bool> {
        self.config.target_config.get(&target)
            .map(|t| t.no_std)
    }

    /// Returns whether the target will be tested using the `remote-test-client`
    /// and `remote-test-server` binaries.
    fn remote_tested(&self, target: Interned<String>) -> bool {
        self.qemu_rootfs(target).is_some() || target.contains("android") ||
        env::var_os("TEST_DEVICE_ADDR").is_some()
    }

    /// Returns the root of the "rootfs" image that this target will be using,
    /// if one was configured.
    ///
    /// If `Some` is returned then that means that tests for this target are
    /// emulated with QEMU and binaries will need to be shipped to the emulator.
    fn qemu_rootfs(&self, target: Interned<String>) -> Option<&Path> {
        self.config.target_config.get(&target)
            .and_then(|t| t.qemu_rootfs.as_ref())
            .map(|p| &**p)
    }

    /// Path to the python interpreter to use
    fn python(&self) -> &Path {
        self.config.python.as_ref().unwrap()
    }

    /// Temporary directory that extended error information is emitted to.
    fn extended_error_dir(&self) -> PathBuf {
        self.out.join("tmp/extended-error-metadata")
    }

    /// Tests whether the `compiler` compiling for `target` should be forced to
    /// use a stage1 compiler instead.
    ///
    /// Currently, by default, the build system does not perform a "full
    /// bootstrap" by default where we compile the compiler three times.
    /// Instead, we compile the compiler two times. The final stage (stage2)
    /// just copies the libraries from the previous stage, which is what this
    /// method detects.
    ///
    /// Here we return `true` if:
    ///
    /// * The build isn't performing a full bootstrap
    /// * The `compiler` is in the final stage, 2
    /// * We're not cross-compiling, so the artifacts are already available in
    ///   stage1
    ///
    /// When all of these conditions are met the build will lift artifacts from
    /// the previous stage forward.
    fn force_use_stage1(&self, compiler: Compiler, target: Interned<String>) -> bool {
        !self.config.full_bootstrap &&
            compiler.stage >= 2 &&
            (self.hosts.iter().any(|h| *h == target) || target == self.build)
    }

    /// Returns the directory that OpenSSL artifacts are compiled into if
    /// configured to do so.
    fn openssl_dir(&self, target: Interned<String>) -> Option<PathBuf> {
        // OpenSSL not used on Windows
        if target.contains("windows") {
            None
        } else if self.config.openssl_static {
            Some(self.out.join(&*target).join("openssl"))
        } else {
            None
        }
    }

    /// Returns the directory that OpenSSL artifacts are installed into if
    /// configured as such.
    fn openssl_install_dir(&self, target: Interned<String>) -> Option<PathBuf> {
        self.openssl_dir(target).map(|p| p.join("install"))
    }

    /// Given `num` in the form "a.b.c" return a "release string" which
    /// describes the release version number.
    ///
    /// For example on nightly this returns "a.b.c-nightly", on beta it returns
    /// "a.b.c-beta.1" and on stable it just returns "a.b.c".
    fn release(&self, num: &str) -> String {
        match &self.config.channel[..] {
            "stable" => num.to_string(),
            "beta" => if self.rust_info.is_git() {
                format!("{}-beta.{}", num, self.beta_prerelease_version())
            } else {
                format!("{}-beta", num)
            },
            "nightly" => format!("{}-nightly", num),
            _ => format!("{}-dev", num),
        }
    }

    fn beta_prerelease_version(&self) -> u32 {
        if let Some(s) = self.prerelease_version.get() {
            return s
        }

        let beta = output(
            Command::new("git")
                .arg("ls-remote")
                .arg("origin")
                .arg("beta")
                .current_dir(&self.src)
        );
        let beta = beta.trim().split_whitespace().next().unwrap();
        let master = output(
            Command::new("git")
                .arg("ls-remote")
                .arg("origin")
                .arg("master")
                .current_dir(&self.src)
        );
        let master = master.trim().split_whitespace().next().unwrap();

        // Figure out where the current beta branch started.
        let base = output(
            Command::new("git")
                .arg("merge-base")
                .arg(beta)
                .arg(master)
                .current_dir(&self.src),
        );
        let base = base.trim();

        // Next figure out how many merge commits happened since we branched off
        // beta. That's our beta number!
        let count = output(
            Command::new("git")
                .arg("rev-list")
                .arg("--count")
                .arg("--merges")
                .arg(format!("{}...HEAD", base))
                .current_dir(&self.src),
        );
        let n = count.trim().parse().unwrap();
        self.prerelease_version.set(Some(n));
        n
    }

    /// Returns the value of `release` above for Rust itself.
    fn rust_release(&self) -> String {
        self.release(channel::CFG_RELEASE_NUM)
    }

    /// Returns the "package version" for a component given the `num` release
    /// number.
    ///
    /// The package version is typically what shows up in the names of tarballs.
    /// For channels like beta/nightly it's just the channel name, otherwise
    /// it's the `num` provided.
    fn package_vers(&self, num: &str) -> String {
        match &self.config.channel[..] {
            "stable" => num.to_string(),
            "beta" => "beta".to_string(),
            "nightly" => "nightly".to_string(),
            _ => format!("{}-dev", num),
        }
    }

    /// Returns the value of `package_vers` above for Rust itself.
    fn rust_package_vers(&self) -> String {
        self.package_vers(channel::CFG_RELEASE_NUM)
    }

    /// Returns the value of `package_vers` above for Cargo
    fn cargo_package_vers(&self) -> String {
        self.package_vers(&self.release_num("cargo"))
    }

    /// Returns the value of `package_vers` above for rls
    fn rls_package_vers(&self) -> String {
        self.package_vers(&self.release_num("rls"))
    }

    /// Returns the value of `package_vers` above for clippy
    fn clippy_package_vers(&self) -> String {
        self.package_vers(&self.release_num("clippy"))
    }

    /// Returns the value of `package_vers` above for rustfmt
    fn rustfmt_package_vers(&self) -> String {
        self.package_vers(&self.release_num("rustfmt"))
    }

    fn llvm_tools_package_vers(&self) -> String {
        self.package_vers(&self.rust_version())
    }

    fn llvm_tools_vers(&self) -> String {
        self.rust_version()
    }

    fn lldb_package_vers(&self) -> String {
        self.package_vers(&self.rust_version())
    }

    fn lldb_vers(&self) -> String {
        self.rust_version()
    }

    fn llvm_link_tools_dynamically(&self, target: Interned<String>) -> bool {
        (target.contains("linux-gnu") || target.contains("apple-darwin"))
    }

    /// Returns the `version` string associated with this compiler for Rust
    /// itself.
    ///
    /// Note that this is a descriptive string which includes the commit date,
    /// sha, version, etc.
    fn rust_version(&self) -> String {
        self.rust_info.version(self, channel::CFG_RELEASE_NUM)
    }

    /// Return the full commit hash
    fn rust_sha(&self) -> Option<&str> {
        self.rust_info.sha()
    }

    /// Returns the `a.b.c` version that the given package is at.
    fn release_num(&self, package: &str) -> String {
        let mut toml = String::new();
        let toml_file_name = self.src.join(&format!("src/tools/{}/Cargo.toml", package));
        t!(t!(File::open(toml_file_name)).read_to_string(&mut toml));
        for line in toml.lines() {
            let prefix = "version = \"";
            let suffix = "\"";
            if line.starts_with(prefix) && line.ends_with(suffix) {
                return line[prefix.len()..line.len() - suffix.len()].to_string()
            }
        }

        panic!("failed to find version in {}'s Cargo.toml", package)
    }

    /// Returns whether unstable features should be enabled for the compiler
    /// we're building.
    fn unstable_features(&self) -> bool {
        match &self.config.channel[..] {
            "stable" | "beta" => false,
            "nightly" | _ => true,
        }
    }

    /// Fold the output of the commands after this method into a group. The fold
    /// ends when the returned object is dropped. Folding can only be used in
    /// the Travis CI environment.
    pub fn fold_output<D, F>(&self, name: F) -> Option<OutputFolder>
        where D: Into<String>, F: FnOnce() -> D
    {
        if !self.config.dry_run && self.ci_env == CiEnv::Travis {
            Some(OutputFolder::new(name().into()))
        } else {
            None
        }
    }

    /// Updates the actual toolstate of a tool.
    ///
    /// The toolstates are saved to the file specified by the key
    /// `rust.save-toolstates` in `config.toml`. If unspecified, nothing will be
    /// done. The file is updated immediately after this function completes.
    pub fn save_toolstate(&self, tool: &str, state: ToolState) {
        use std::io::{Seek, SeekFrom};

        if let Some(ref path) = self.config.save_toolstates {
            let mut file = t!(fs::OpenOptions::new()
                .create(true)
                .read(true)
                .write(true)
                .open(path));

            let mut current_toolstates: HashMap<Box<str>, ToolState> =
                serde_json::from_reader(&mut file).unwrap_or_default();
            current_toolstates.insert(tool.into(), state);
            t!(file.seek(SeekFrom::Start(0)));
            t!(file.set_len(0));
            t!(serde_json::to_writer(file, &current_toolstates));
        }
    }

    fn in_tree_crates(&self, root: &str) -> Vec<&Crate> {
        let mut ret = Vec::new();
        let mut list = vec![INTERNER.intern_str(root)];
        let mut visited = HashSet::new();
        while let Some(krate) = list.pop() {
            let krate = &self.crates[&krate];
            if krate.is_local(self) {
                ret.push(krate);
                for dep in &krate.deps {
                    if visited.insert(dep) && dep != "build_helper" {
                        list.push(*dep);
                    }
                }
            }
        }
        ret
    }

    fn read_stamp_file(&self, stamp: &Path) -> Vec<PathBuf> {
        if self.config.dry_run {
            return Vec::new();
        }

        let mut paths = Vec::new();
        let mut contents = Vec::new();
        t!(t!(File::open(stamp)).read_to_end(&mut contents));
        // This is the method we use for extracting paths from the stamp file passed to us. See
        // run_cargo for more information (in compile.rs).
        for part in contents.split(|b| *b == 0) {
            if part.is_empty() {
                continue
            }
            let path = PathBuf::from(t!(str::from_utf8(part)));
            paths.push(path);
        }
        paths
    }

    /// Copies a file from `src` to `dst`
    pub fn copy(&self, src: &Path, dst: &Path) {
        if self.config.dry_run { return; }
        let _ = fs::remove_file(&dst);
        let metadata = t!(src.symlink_metadata());
        if metadata.file_type().is_symlink() {
            let link = t!(fs::read_link(src));
            t!(symlink_file(link, dst));
        } else if let Ok(()) = fs::hard_link(src, dst) {
            // Attempt to "easy copy" by creating a hard link
            // (symlinks don't work on windows), but if that fails
            // just fall back to a slow `copy` operation.
        } else {
            if let Err(e) = fs::copy(src, dst) {
                panic!("failed to copy `{}` to `{}`: {}", src.display(),
                       dst.display(), e)
            }
            t!(fs::set_permissions(dst, metadata.permissions()));
            let atime = FileTime::from_last_access_time(&metadata);
            let mtime = FileTime::from_last_modification_time(&metadata);
            t!(filetime::set_file_times(dst, atime, mtime));
        }
    }

    /// Search-and-replaces within a file. (Not maximally efficiently: allocates a
    /// new string for each replacement.)
    pub fn replace_in_file(&self, path: &Path, replacements: &[(&str, &str)]) {
        if self.config.dry_run { return; }
        let mut contents = String::new();
        let mut file = t!(OpenOptions::new().read(true).write(true).open(path));
        t!(file.read_to_string(&mut contents));
        for &(target, replacement) in replacements {
            contents = contents.replace(target, replacement);
        }
        t!(file.seek(SeekFrom::Start(0)));
        t!(file.set_len(0));
        t!(file.write_all(contents.as_bytes()));
    }

    /// Copies the `src` directory recursively to `dst`. Both are assumed to exist
    /// when this function is called.
    pub fn cp_r(&self, src: &Path, dst: &Path) {
        if self.config.dry_run { return; }
        for f in t!(fs::read_dir(src)) {
            let f = t!(f);
            let path = f.path();
            let name = path.file_name().unwrap();
            let dst = dst.join(name);
            if t!(f.file_type()).is_dir() {
                t!(fs::create_dir_all(&dst));
                self.cp_r(&path, &dst);
            } else {
                let _ = fs::remove_file(&dst);
                self.copy(&path, &dst);
            }
        }
    }

    /// Copies the `src` directory recursively to `dst`. Both are assumed to exist
    /// when this function is called. Unwanted files or directories can be skipped
    /// by returning `false` from the filter function.
    pub fn cp_filtered(&self, src: &Path, dst: &Path, filter: &dyn Fn(&Path) -> bool) {
        // Immediately recurse with an empty relative path
        self.recurse_(src, dst, Path::new(""), filter)
    }

    // Inner function does the actual work
    fn recurse_(&self, src: &Path, dst: &Path, relative: &Path, filter: &dyn Fn(&Path) -> bool) {
        for f in self.read_dir(src) {
            let path = f.path();
            let name = path.file_name().unwrap();
            let dst = dst.join(name);
            let relative = relative.join(name);
            // Only copy file or directory if the filter function returns true
            if filter(&relative) {
                if t!(f.file_type()).is_dir() {
                    let _ = fs::remove_dir_all(&dst);
                    self.create_dir(&dst);
                    self.recurse_(&path, &dst, &relative, filter);
                } else {
                    let _ = fs::remove_file(&dst);
                    self.copy(&path, &dst);
                }
            }
        }
    }

    fn copy_to_folder(&self, src: &Path, dest_folder: &Path) {
        let file_name = src.file_name().unwrap();
        let dest = dest_folder.join(file_name);
        self.copy(src, &dest);
    }

    fn install(&self, src: &Path, dstdir: &Path, perms: u32) {
        if self.config.dry_run { return; }
        let dst = dstdir.join(src.file_name().unwrap());
        t!(fs::create_dir_all(dstdir));
        drop(fs::remove_file(&dst));
        {
            let mut s = t!(fs::File::open(&src));
            let mut d = t!(fs::File::create(&dst));
            io::copy(&mut s, &mut d).expect("failed to copy");
        }
        chmod(&dst, perms);
    }

    fn create(&self, path: &Path, s: &str) {
        if self.config.dry_run { return; }
        t!(fs::write(path, s));
    }

    fn read(&self, path: &Path) -> String {
        if self.config.dry_run { return String::new(); }
        t!(fs::read_to_string(path))
    }

    fn create_dir(&self, dir: &Path) {
        if self.config.dry_run { return; }
        t!(fs::create_dir_all(dir))
    }

    fn remove_dir(&self, dir: &Path) {
        if self.config.dry_run { return; }
        t!(fs::remove_dir_all(dir))
    }

    fn read_dir(&self, dir: &Path) -> impl Iterator<Item=fs::DirEntry> {
        let iter = match fs::read_dir(dir) {
            Ok(v) => v,
            Err(_) if self.config.dry_run => return vec![].into_iter(),
            Err(err) => panic!("could not read dir {:?}: {:?}", dir, err),
        };
        iter.map(|e| t!(e)).collect::<Vec<_>>().into_iter()
    }

    fn remove(&self, f: &Path) {
        if self.config.dry_run { return; }
        fs::remove_file(f).unwrap_or_else(|_| panic!("failed to remove {:?}", f));
    }
}
