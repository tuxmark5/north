use {
  crate::{
    mir::{self, RValLink},
  },
  north_core::Model,
  serde::{Serialize, ser},
  std::{
    error,
    fmt::{self, Display},
    result
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////

pub type Result<T> = result::Result<T, Error>;

////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Debug)]
pub struct Error { }

impl Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "err")
  }
}

impl error::Error for Error { }

impl ser::Error for Error {
  fn custom<T: Display>(_msg: T) -> Self {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Serializer<'m> {
  model: &'m mut Model
}

impl<'m> Serializer<'m> {
  fn const_int(&mut self, bit_width: u32, v: u64) -> Result<RValLink> {
    let ty = self.model.build_node(mir::TypeInt { bit_width }).into();
    let value = self.model.build_node(mir::ConstInt { ty, value: v as i64 }).up().into();
    Ok(value)
  }
}

impl<'s, 'm> ser::Serializer for &'s mut Serializer<'m> {
  type Ok = RValLink;
  type Error = Error;

  type SerializeMap = SerializeMap;
  type SerializeSeq = SerializeSeq;
  type SerializeStruct = SerializeStruct<'s, 'm>;
  type SerializeStructVariant = SerializeStructVariant;
  type SerializeTuple = SerializeTuple;
  type SerializeTupleStruct = SerializeTupleStruct;
  type SerializeTupleVariant = SerializeTupleVariant;

  fn serialize_bool(self, v: bool) -> Result<Self::Ok> {
    self.const_int(1, v as u64)
  }

  fn serialize_char(self, _v: char) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_f32(self, _v: f32) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_f64(self, _v: f64) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_i8(self, v: i8) -> Result<Self::Ok> {
    self.const_int(8, v as u64)
  }

  fn serialize_i16(self, v: i16) -> Result<Self::Ok> {
    self.const_int(16, v as u64)
  }

  fn serialize_i32(self, v: i32) -> Result<Self::Ok> {
    self.const_int(32, v as u64)
  }

  fn serialize_i64(self, v: i64) -> Result<Self::Ok> {
    self.const_int(64, v as u64)
  }

  fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
    unimplemented!()
  }

  fn serialize_newtype_struct<T: Serialize + ?Sized>(
    self,
    _name: &'static str,
    _value: &T
  ) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_newtype_variant<T: Serialize + ?Sized>(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _value: &T
  ) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_none(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
    unimplemented!()
  }

  fn serialize_some<T: Serialize + ?Sized>(self, _v: &T) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_str(self, _v: &str) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_struct(
    self,
    _name: &'static str,
    _len: usize
  ) -> Result<Self::SerializeStruct> {
    Ok(SerializeStruct {
      parent: self,
      members: Vec::new(),
    })
  }

  fn serialize_struct_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _len: usize
  ) -> Result<Self::SerializeStructVariant> {
    unimplemented!()
  }

  fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
    unimplemented!()
  }

  fn serialize_tuple_struct(
    self,
    _name: &'static str,
    _len: usize
  ) -> Result<Self::SerializeTupleStruct> {
    unimplemented!()
  }

  fn serialize_tuple_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str,
    _len: usize
  ) -> Result<Self::SerializeTupleVariant> {
    unimplemented!()
  }

  fn serialize_u8(self, v: u8) -> Result<Self::Ok> {
    self.const_int(8, v as u64)
  }

  fn serialize_u16(self, v: u16) -> Result<Self::Ok> {
    self.const_int(16, v as u64)
  }

  fn serialize_u32(self, v: u32) -> Result<Self::Ok> {
    self.const_int(32, v as u64)
  }

  fn serialize_u64(self, v: u64) -> Result<Self::Ok> {
    self.const_int(64, v as u64)
  }

  fn serialize_unit(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_unit_variant(
    self,
    _name: &'static str,
    _variant_index: u32,
    _variant: &'static str
  ) -> Result<Self::Ok> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeMap { }

impl ser::SerializeMap for SerializeMap {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_key<T: Serialize + ?Sized>(&mut self, _key: &T) -> Result<()> {
    unimplemented!()
  }

  fn serialize_value<T: Serialize + ?Sized>(&mut self, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeSeq { }

impl ser::SerializeSeq for SerializeSeq {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_element<T: ?Sized>(&mut self, _value: &T) -> Result<()> where
    T: Serialize
  {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeStruct<'s, 'm: 's> {
  parent: &'s mut Serializer<'m>,
  members: Vec<RValLink>
}

impl<'s, 'm> ser::SerializeStruct for SerializeStruct<'s, 'm> {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(
    &mut self,
    _key: &'static str,
    value: &T
  ) -> Result<()> {
    let mut ser = Serializer { model: self.parent.model };
    let value = value.serialize(&mut ser)?;
    self.members.push(value);
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeStructVariant { }

impl ser::SerializeStructVariant for SerializeStructVariant {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(&mut self, _key: &str, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeTuple { }

impl ser::SerializeTuple for SerializeTuple {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_element<T: Serialize + ?Sized>(&mut self, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeTupleStruct { }

impl ser::SerializeTupleStruct for SerializeTupleStruct {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(&mut self, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeTupleVariant { }

impl ser::SerializeTupleVariant for SerializeTupleVariant {
  type Ok = RValLink;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(&mut self, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn quote_value<T: Serialize + ?Sized>(model: &mut Model, v: &T) -> Result<RValLink> {
  let mut ser = Serializer { model };
  v.serialize(&mut ser)
}

////////////////////////////////////////////////////////////////////////////////////////////////
