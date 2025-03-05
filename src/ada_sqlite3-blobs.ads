-------------------------------------------------------------------------------
-- Ada_Sqlite3.Blobs - BLOB handling for SQLite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Finalization;

package Ada_Sqlite3.Blobs is

   --  BLOB data type
   type Blob is private;

   --  Create a new BLOB from a byte array
   function Create (Data : Ada.Streams.Stream_Element_Array) return Blob;

   --  Get the size of a BLOB in bytes
   function Size (Object : Blob) return Natural;

   --  Get the raw data of a BLOB
   function Data (Object : Blob) return Ada.Streams.Stream_Element_Array;

   --  Bind a BLOB value to a parameter
   procedure Bind_Blob
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Blob);

   --  Get a BLOB value from a column
   function Column_Blob
     (Stmt  : Statement;
      Index : Natural) return Blob;

   --  Stream-like operations
   procedure Read_Blob
     (B     : in out Blob;
      Item  : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset);

   procedure Write_Blob
     (B     : in out Blob;
      Item  : Ada.Streams.Stream_Element_Array);

private

   type Blob_Data_Access is access Ada.Streams.Stream_Element_Array;
   
   type Blob is new Ada.Finalization.Controlled with record
      Data   : Blob_Data_Access := null;
      Cursor : Ada.Streams.Stream_Element_Offset := 0;
   end record;

   overriding procedure Initialize (Object : in out Blob);
   overriding procedure Adjust (Object : in out Blob);
   overriding procedure Finalize (Object : in out Blob);

end Ada_Sqlite3.Blobs;
