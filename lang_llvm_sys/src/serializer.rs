use {
  crate::{
    context::Context,
  },
  llvm_sys::{
    core as llvm_core,
    prelude::*
  },
  serde::{Serialize, ser},
  std::{
    error, ptr, result,
    fmt::{self, Display},
  },
};

////////////////////////////////////////////////////////////////////////////////////////////////
static NATIVE_TAG: &'static str = "##NATIVE_TAG";
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

#[derive(Clone)]
pub struct Native(pub LLVMValueRef);

impl Serialize for Native {
  fn serialize<S>(&self, serializer: S) 
    -> result::Result<S::Ok, S::Error> where
    S: ser::Serializer
  {
    let value = NativeVal(self.0);
    serializer.serialize_newtype_struct(NATIVE_TAG, &value)
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

struct NativeVal(LLVMValueRef);

impl Serialize for NativeVal {
  fn serialize<S>(&self, _serializer: S)
    -> result::Result<S::Ok, S::Error> where
    S: ser::Serializer
  { 
    panic!("can't serialize Native LLVM value with non-LLVM serializer")
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub type Result<T> = result::Result<T, Error>;

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Serializer<'c> {
  context: &'c Context
}

impl<'c> Serializer<'c> {
  fn const_int(&self, bit_width: u32, v: u64) -> Result<LLVMValueRef> {
    unsafe {
      let ty = self.context.type_int(bit_width);
      let value = llvm_core::LLVMConstInt(ty, v as u64, 0);
      Ok(value)
    }
  }
}

impl<'c> ser::Serializer for &'c Serializer<'c> {
  type Ok = LLVMValueRef;
  type Error = Error;

  type SerializeMap = SerializeMap;
  type SerializeSeq = SerializeSeq<'c>;
  type SerializeStruct = SerializeStruct<'c>;
  type SerializeStructVariant = SerializeStructVariant;
  type SerializeTuple = SerializeTuple<'c>;
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
    name: &'static str,
    value: &T
  ) -> Result<Self::Ok> {
    match ptr::eq(name, NATIVE_TAG) {
      true => unsafe { 
        let native = value as *const T as *const NativeVal;
        Ok((*native).0)
      },
      false => {
        value.serialize(self)
      }
    }
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
    Ok(SerializeSeq {
      parent: self,
      members: Vec::new(),
    })
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
    Ok(SerializeTuple {
      parent: self,
      members: Vec::new(),
    })
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
    Ok(ptr::null_mut())
  }

  fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok> {
    Ok(ptr::null_mut())
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
  type Ok = LLVMValueRef;
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

pub struct SerializeSeq<'s> {
  parent: &'s Serializer<'s>,
  members: Vec<LLVMValueRef>
}

impl<'s> ser::SerializeSeq for SerializeSeq<'s> {
  type Ok = LLVMValueRef;
  type Error = Error;

  fn end(mut self) -> Result<Self::Ok> {
    unsafe {
      let elem_ty = match self.members.get_mut(0) {
        Some(first) => llvm_core::LLVMTypeOf(*first),
        None => llvm_core::LLVMIntType(8),
      };

      Ok(llvm_core::LLVMConstArray(
        elem_ty,
        self.members.as_mut_ptr(),
        self.members.len() as u32
      ))
    }
  }

  fn serialize_element<T: ?Sized>(&mut self, value: &T) -> Result<()> where
    T: Serialize
  {
    let value = value.serialize(self.parent)?;
    self.members.push(value);
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeStruct<'s> {
  parent: &'s Serializer<'s>,
  members: Vec<LLVMValueRef>
}

impl<'s> ser::SerializeStruct for SerializeStruct<'s> {
  type Ok = LLVMValueRef;
  type Error = Error;

  fn end(mut self) -> Result<Self::Ok> {
    unsafe {
      let ctx_raw = self.parent.context.raw();
      let members = self.members.as_mut_ptr();
      let count = self.members.len() as u32;
      let value = llvm_core::LLVMConstStructInContext(ctx_raw, members, count, 0);
      Ok(value)
    }
  }

  fn serialize_field<T: Serialize + ?Sized>(
    &mut self,
    _key: &'static str,
    value: &T
  ) -> Result<()> {
    let value = value.serialize(self.parent)?;
    self.members.push(value);
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeStructVariant { }

impl ser::SerializeStructVariant for SerializeStructVariant {
  type Ok = LLVMValueRef;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(&mut self, _key: &str, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeTuple<'s> { 
  parent: &'s Serializer<'s>,
  members: Vec<LLVMValueRef>
}

impl<'s> ser::SerializeTuple for SerializeTuple<'s> {
  type Ok = LLVMValueRef;
  type Error = Error;

  fn end(mut self) -> Result<Self::Ok> {
    unsafe {
      let ctx_raw = self.parent.context.raw();
      let members = self.members.as_mut_ptr();
      let count = self.members.len() as u32;
      let value = llvm_core::LLVMConstStructInContext(ctx_raw, members, count, 0);
      Ok(value)
    }
  }

  fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<()> {
    let value = value.serialize(self.parent)?;
    self.members.push(value);
    Ok(())
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub struct SerializeTupleStruct { }

impl ser::SerializeTupleStruct for SerializeTupleStruct {
  type Ok = LLVMValueRef;
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
  type Ok = LLVMValueRef;
  type Error = Error;

  fn end(self) -> Result<Self::Ok> {
    unimplemented!()
  }

  fn serialize_field<T: Serialize + ?Sized>(&mut self, _value: &T) -> Result<()> {
    unimplemented!()
  }
}

////////////////////////////////////////////////////////////////////////////////////////////////

pub fn serialize<T: Serialize + ?Sized>(context: &Context, v: &T) -> Result<LLVMValueRef> {
  let ser = Serializer { context };
  v.serialize(&ser)
}

////////////////////////////////////////////////////////////////////////////////////////////////
