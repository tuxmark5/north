use {
  clap::{App, clap_app},
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct GlobalSettings {
  pub grammar_file: String,
  pub input_file: String,
  pub dump_evm_stats: bool,
  pub enable_gc: bool,
  pub incorporate_reductions: bool,
  pub profile: bool,
  pub show_mir: bool,
  pub show_opt_mir: bool,
  pub show_parse_tree: bool,
  pub show_reductions: bool,
}

impl GlobalSettings {
  pub fn parse_cmd_line() -> Option<Self> {
    let app = command_line();
    let matches = app.get_matches();

    let parse_m = matches.subcommand_matches("parse")?;
    Some(GlobalSettings {
      grammar_file: parse_m.value_of("GRAMMAR")?.to_owned(),
      input_file: parse_m.value_of("INPUT")?.to_owned(),
      dump_evm_stats: parse_m.is_present("dump_evm_stats"),
      enable_gc: !parse_m.is_present("disable_gc"),
      incorporate_reductions: !parse_m.is_present("no_red_incorp"),
      profile: parse_m.is_present("profile"),
      show_mir: parse_m.is_present("show_mir"),
      show_opt_mir: parse_m.is_present("show_opt_mir"),
      show_parse_tree: parse_m.is_present("show_parse_tree"),
      show_reductions: parse_m.is_present("show_reductions"),
    })
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

fn command_line<'a, 'b>() -> App<'a, 'b> {
  clap_app!(north_test =>
    (about: "command line utility for EVM parser")
    (version: "0.1")

    (@subcommand parse =>
      (@arg GRAMMAR: -g --grammar +required +takes_value "Grammar file")
      (@arg INPUT: -i --input +required +takes_value "Input file to parse")

      (@arg disable_gc: -G --no_gc "Disable garbage collector")
      (@arg dump_evm_stats: -e --evm_stats "Show EVM parser stats")
      (@arg no_red_incorp: -I --no_incorp "Disable reduction incorporation")
      (@arg profile: -p --profile "Show profiler measurements")
      (@arg show_mir: -m --show_mir "Show MIR model")
      (@arg show_opt_mir: -o --show_opt_mir "Show on-the-fly generated optimized MIR")
      (@arg show_parse_tree: -t --show_tree "Show parse tree")
      (@arg show_reductions: -r --show_reduce "Show parser reductions")
    )
  )
}

////////////////////////////////////////////////////////////////////////////////////////////////
