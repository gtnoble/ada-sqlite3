-------------------------------------------------------------------------------
-- Ada_Sqlite3.Low_Level - Low-level SQLite3 C API bindings
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Ada_Sqlite3.Low_Level is

   function To_Chars_Ptr is new Ada.Unchecked_Conversion(
     Source => System.Address,
     Target => CS.chars_ptr);

   --  Convert SQLite3 datatype to Ada_Sqlite3 column type
   function To_Column_Type (Value : Datatype) return Column_Type is
   begin
      case Value is
         when SQLITE_INTEGER => return Integer_Type;
         when SQLITE_FLOAT   => return Float_Type;
         when SQLITE_TEXT    => return Text_Type;
         when SQLITE_BLOB    => return Blob_Type;
         when SQLITE_NULL    => return Null_Type;
      end case;
   end To_Column_Type;

   --  Convert Ada_Sqlite3 column type to SQLite3 datatype
   function To_Datatype (Value : Column_Type) return Datatype is
   begin
      case Value is
         when Integer_Type => return SQLITE_INTEGER;
         when Float_Type   => return SQLITE_FLOAT;
         when Text_Type    => return SQLITE_TEXT;
         when Blob_Type    => return SQLITE_BLOB;
         when Null_Type    => return SQLITE_NULL;
      end case;
   end To_Datatype;

   procedure Free_Chars_Ptr (Arg : System.Address) is
      Ptr : CS.chars_ptr := To_Chars_Ptr (Arg);
   begin
      CS.Free (Ptr);
   end Free_Chars_Ptr;

   function Free_Chars_Ptr_Address return Destructor_Type is
   begin
      return Destructor_Type (Free_Chars_Ptr'Address);
   end Free_Chars_Ptr_Address;

   procedure Free_Memory (Arg : System.Address) is
      type Byte_Access is access all C.char;
      procedure Free is new Ada.Unchecked_Deallocation(C.char, Byte_Access);
      
      function To_Byte_Access is new Ada.Unchecked_Conversion(
        Source => System.Address,
        Target => Byte_Access);
      
      Ptr : Byte_Access := To_Byte_Access(Arg);
   begin
      Free(Ptr);
   end Free_Memory;

   function Free_Memory_Address return Destructor_Type is
   begin
      return Destructor_Type(Free_Memory'Address);
   end Free_Memory_Address;

   function Copy_Blob_Data (Data : System.Address; Size : C.int) return System.Address is
      subtype Blob_Data is C.char_array(1..C.size_t(Size));
      type Blob_Access is access all Blob_Data;
      
      Source : Blob_Data;
      for Source'Address use Data;
      
      Copy : constant Blob_Access := new Blob_Data'(Source);
   begin
      return Copy.all'Address;
   end Copy_Blob_Data;

end Ada_Sqlite3.Low_Level;
