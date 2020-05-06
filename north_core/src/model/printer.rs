use {
  crate::{
    Node, NodeId,
    model::{
      Model,
      member::{
        ChildLink, Member, ReferenceLink, TokenMember
      },
    },
    structure::StorageExt,
    util::downcast::DowncastExt,
  },
  std::{
    collections::{
      HashMap, HashSet,
      hash_map
    },
    fmt::{self, Write},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

trait ModelVisitor {
  type Error;

  fn visit_member(&mut self, parent: &dyn Node, member: &dyn Member)
    -> Result<(), Self::Error>
  {
    if let Some(link) = member.downcast_ref::<dyn ChildLink>() {
      self.visit_member_child(parent, link)
    } else if let Some(link) = member.downcast_ref::<dyn ReferenceLink>() {
      self.visit_member_reference(parent, link)
    } else if let Some(member) = member.downcast_ref::<dyn TokenMember>() {
      self.visit_member_token(parent, member)
    } else {
      self.visit_member_any(parent, member)
    }
  }

  fn visit_member_any(&mut self, _node: &dyn Node, _link: &dyn Member)
    -> Result<(), Self::Error> { Ok(()) }
  fn visit_member_child(&mut self, _node: &dyn Node, _link: &dyn ChildLink)
    -> Result<(), Self::Error> { Ok(()) }
  fn visit_member_reference(&mut self, _node: &dyn Node, _link: &dyn ReferenceLink)
    -> Result<(), Self::Error> { Ok(()) }
  fn visit_member_token(&mut self, _node: &dyn Node, _link: &dyn TokenMember)
    -> Result<(), Self::Error> { Ok(()) }

  fn visit_node(&mut self, _id: NodeId, node: &dyn Node) -> Result<(), Self::Error> {
    let concept = node.instance_concept();
    for member in concept.members() {
      self.visit_member(node, *member)?;
    }
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Eq, PartialEq)]
enum NodeRole {
  Child,
  Root
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct PrepareVisitor<'p, 'm: 'p> {
  printer: &'p mut Printer<'m>,
  queue: Vec<NodeId>,
  visited_nodes: HashSet<NodeId>,
}

impl<'p, 'm> ModelVisitor for PrepareVisitor<'p, 'm> {
  type Error = ();

  fn visit_member_child(&mut self, node: &dyn Node, link: &dyn ChildLink) -> Result<(), ()> {
    for child_id in link.link_nodes(node) {
      let child = self.printer.model.data(child_id);
      let role = if self.printer.flat_mode { NodeRole::Root } else { NodeRole::Child };
      self.printer.node_roles.insert(child_id, role);
      self.visit_node(child_id, &*child)?;
    }

    Ok(())
  }

  fn visit_member_reference(&mut self, node: &dyn Node, link: &dyn ReferenceLink)
    -> Result<(), ()>
  {
    let target_node = match *link.target_node(node) {
      Some(target_node) => target_node,
      None => return Ok(()),
    };

    self.printer.referenced_nodes.insert(target_node);

    use self::hash_map::Entry::Vacant;
    if let Vacant(vacant) = self.printer.node_roles.entry(target_node) {
      vacant.insert(NodeRole::Root);
      self.queue.push(target_node);
    }

    Ok(())
  }

  fn visit_node(&mut self, id: NodeId, node: &dyn Node) -> Result<(), ()> {
    if !self.visited_nodes.insert(id) {
      return Ok(());
    }

    let concept = node.instance_concept();
    for member in concept.members() {
      self.visit_member(node, *member)?;
    }

    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct PrintVisitor<'p, 'm: 'p, 'w> {
  indent_level: usize,
  line_start: bool,
  printer: &'p Printer<'m>,
  writer: &'w mut dyn Write,
}

impl<'p, 'm, 'w> PrintVisitor<'p, 'm, 'w> {
  fn start_field(&mut self, name: &str) -> fmt::Result {
    write!(self, "{}: ", name)
  }

  fn write_block<F>(&mut self, f: F) -> fmt::Result where
    F: FnOnce(&mut Self) -> fmt::Result
  {
    write!(self, " {{")?;
    self.write_nl()?;
    self.indent_level += 1;
    f(self)?;
    self.indent_level -= 1;
    write!(self, "}}")?;
    self.write_nl()
  }

  fn write_indent(&mut self) -> fmt::Result {
    for _ in 0..self.indent_level {
      self.writer.write_str("    ")?;
    }
    Ok(())
  }

  fn write_nl(&mut self) -> fmt::Result {
    if !self.line_start {
      self.writer.write_char('\n')?;
      self.line_start = true;
    }
    Ok(())
  }
}

impl<'p, 'm, 'w> ModelVisitor for PrintVisitor<'p, 'm, 'w> {
  type Error = fmt::Error;

  fn visit_member_any(&mut self, node: &dyn Node, link: &dyn Member) -> fmt::Result {
    let data = link.debug(node);
    self.start_field(link.name())?;
    write!(self, "{:?}", data)?;
    self.write_nl()
  }

  fn visit_member_child(&mut self, node: &dyn Node, link: &dyn ChildLink) -> fmt::Result {
    for (idx, child_id) in link.link_nodes(node).enumerate() {
      let child = self.printer.model.data(child_id);
      write!(self, "{}[{}]: ", link.name(), idx)?;
      if self.printer.flat_mode {
        write!(self, "#{:?}", child_id)?;
        self.write_nl()?;
      } else {
        self.visit_node(child_id, &*child)?;
      }
    }
    Ok(())
  }

  fn visit_member_reference(&mut self, node: &dyn Node, link: &dyn ReferenceLink)
    -> fmt::Result
  {
    let ref_obj_debug = link.reference_obj_debug(node);
    let target_node_id = *link.target_node(node);
    self.start_field(link.name())?;
    match target_node_id {
      Some(id) => write!(self, "{:?} => #{:?}", ref_obj_debug, id)?,
      None => write!(self, "{:?} => #INV", ref_obj_debug)?,
    }
    self.write_nl()
  }

  fn visit_member_token(&mut self, node: &dyn Node, link: &dyn TokenMember) -> fmt::Result {
    let token = link.debug_token(node);
    self.start_field(link.name())?;
    write!(self, "{:?}", token)?;
    self.write_nl()
  }

  fn visit_node(&mut self, id: NodeId, node: &dyn Node) -> fmt::Result {
    let concept = node.instance_concept();
    //let is_root = self.printer.node_roles[&id] == NodeRole::Root;
    //let is_ref = self.printer.referenced_nodes.contains(&id);

    //if true || is_root || is_ref {
    write!(self, "#{:?}: ", id)?;
    //}

    // abort here if already printed

    write!(self, "{}", concept.name())?;
    self.write_block(|self_| {
      for member in concept.members() {
        self_.visit_member(node, *member)?;
      }
      Ok(())
    })
  }
}

impl<'p, 'm, 'w> Write for PrintVisitor<'p, 'm, 'w> {
  fn write_str(&mut self, s: &str) -> Result<(), fmt::Error> {
    if self.line_start {
      self.write_indent()?;
      self.line_start = false;
    }
    self.writer.write_str(s)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Printer<'m> {
  flat_mode: bool,
  model: &'m Model,
  node_roles: HashMap<NodeId, NodeRole>,
  referenced_nodes: HashSet<NodeId>,
}

impl<'m> Printer<'m> {
  pub fn new(model: &'m Model) -> Self {
    Self {
      flat_mode: false,
      model,
      node_roles: HashMap::new(),
      referenced_nodes: HashSet::new(),
    }
  }

  pub fn add_all(&mut self) {
    for key in self.model.storage().data.keys() {
      let entry = self.node_roles.entry(*key);
      entry.or_insert(NodeRole::Root);
    }
  }

  pub fn add_root(&mut self, node_id: NodeId) {
    self.node_roles.insert(node_id, NodeRole::Root);
  }

  pub fn prepare(&mut self) {
    let model = self.model;

    let queue = self.node_roles.keys()
      .cloned()
      .collect();

    let mut visitor = PrepareVisitor {
      printer: self,
      queue,
      visited_nodes: HashSet::new(),
    };

    while let Some(node_id) = visitor.queue.pop() {
      let node_ref = model.data(node_id);
      visitor.visit_node(node_id, &*node_ref).unwrap();
    }
  }

  pub fn print(&mut self, writer: &mut dyn Write) {
    let mut visitor = PrintVisitor {
      indent_level: 0,
      line_start: true,
      printer: self,
      writer,
    };

    let mut root_nodes = self.node_roles.iter()
      .filter(|(_, v)| **v == NodeRole::Root)
      .map(|(k, _)| *k)
      .collect::<Vec<_>>();
    root_nodes.sort();

    for node_id in root_nodes {
      let node_ref = self.model.data(node_id);
      visitor.visit_node(node_id, &*node_ref).unwrap();
    }
  }

  pub fn print_model(model: &'m Model, writer: &mut dyn Write) {
    let mut printer = Printer::new(model);
    printer.add_all();
    printer.prepare();
    printer.print(writer);
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////
