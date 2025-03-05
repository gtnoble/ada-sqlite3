-------------------------------------------------------------------------------
-- Ada_Sqlite3.Blobs - BLOB handling for SQLite3
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Ada_Sqlite3.Low_Level;
with System;

package body Ada_Sqlite3.Blobs is

   package C renames Interfaces.C;
   package LL renames Ada_Sqlite3.Low_Level;
   package SEA renames Ada.Streams;

   use type C.int;
   use type System.Address;

   procedure Free is new Ada.Unchecked_Deallocation
     (SEA.Stream_Element_Array, Blob_Data_Access);

   --  Create a new BLOB from a byte array
   function Create (Data : Ada.Streams.Stream_Element_Array) return Blob is
      Result : Blob;
   begin
      Result.Data := new Ada.Streams.Stream_Element_Array'(Data);
      Result.Cursor := Data'First;
      return Result;
   end Create;

   --  Get the size of a BLOB in bytes
   function Size (Object : Blob) return Natural is
   begin
      if Object.Data = null then
         return 0;
      else
         return Object.Data'Length;
      end if;
   end Size;

   --  Get the raw data of a BLOB
   function Data (Object : Blob) return Ada.Streams.Stream_Element_Array is
   begin
      if Object.Data = null then
         return (1 .. 0 => <>);
      else
         return Object.Data.all;
      end if;
   end Data;

   --  Bind a BLOB value to a parameter
   procedure Bind_Blob
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Blob)
   is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      if Value.Data = null then
         --  Bind NULL if the BLOB is empty
         Result := LL.Sqlite3_Bind_Null
           (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
            Index => C.int (Index));
      else
         --  Bind the BLOB data
         Result := LL.Sqlite3_Bind_Blob
           (Stmt      => LL.Sqlite3_Stmt (Stmt.Handle),
            Index     => C.int (Index),
            Value     => Value.Data.all'Address,
            N_Bytes   => C.int (Value.Data'Length),
            Destructor => LL.SQLITE_TRANSIENT);
      end if;

      if Result /= OK then
         Raise_Error (Result, "Failed to bind BLOB value");
      end if;
   end Bind_Blob;

   --  Get a BLOB value from a column
   function Column_Blob
     (Stmt  : Statement;
      Index : Natural) return Blob
   is
      Blob_Ptr : System.Address;
      Blob_Size : C.int;
      Result : Blob;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      --  Check if the column is NULL
      if Get_Column_Type (Stmt, Index) = Null_Type then
         return Result;  --  Return an empty BLOB
      end if;

      --  Get the BLOB data
      Blob_Ptr := LL.Sqlite3_Column_Blob
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      --  Get the BLOB size
      Blob_Size := LL.Sqlite3_Column_Bytes
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      if Blob_Ptr = System.Null_Address or Blob_Size <= 0 then
         return Result;  --  Return an empty BLOB
      end if;

      --  Copy the BLOB data
      declare
         subtype Blob_Array is Ada.Streams.Stream_Element_Array(1 .. Ada.Streams.Stream_Element_Offset(Blob_Size));
         type Blob_Array_Access is access all Blob_Array;
         function To_Blob_Array_Access is new Ada.Unchecked_Conversion
           (System.Address, Blob_Array_Access);
         Blob_Data : constant Blob_Array_Access := To_Blob_Array_Access(Blob_Ptr);
      begin
         Result.Data := new Ada.Streams.Stream_Element_Array'(Blob_Data.all);
         Result.Cursor := Result.Data'First;
      end;

      return Result;
   end Column_Blob;

   --  Stream operations
   procedure Read_Blob
     (B     : in out Blob;
      Item  : out Ada.Streams.Stream_Element_Array;
      Last  : out Ada.Streams.Stream_Element_Offset)
   is
      use type Ada.Streams.Stream_Element_Offset;
   begin
      if B.Data = null then
         Last := Item'First - 1;
         return;
      end if;

      if B.Cursor > B.Data'Last then
         Last := Item'First - 1;
         return;
      end if;

      declare
         Remaining : constant Ada.Streams.Stream_Element_Offset :=
           B.Data'Last - B.Cursor + 1;
         Copy_Count : constant Ada.Streams.Stream_Element_Offset :=
           Ada.Streams.Stream_Element_Offset'Min (Item'Length, Remaining);
      begin
         Item (Item'First .. Item'First + Copy_Count - 1) :=
           B.Data (B.Cursor .. B.Cursor + Copy_Count - 1);
         B.Cursor := B.Cursor + Copy_Count;
         Last := Item'First + Copy_Count - 1;
      end;
   end Read_Blob;

   procedure Write_Blob
     (B     : in out Blob;
      Item  : Ada.Streams.Stream_Element_Array)
   is
      use type Ada.Streams.Stream_Element_Offset;
      New_Data : Blob_Data_Access;
   begin
      if B.Data = null then
         B.Data := new Ada.Streams.Stream_Element_Array'(Item);
         B.Cursor := B.Data'First;
      else
         --  Append to existing data
         New_Data := new Ada.Streams.Stream_Element_Array
           (B.Data'First .. B.Data'Last + Item'Length);
         New_Data (B.Data'Range) := B.Data.all;
         New_Data (B.Data'Last + 1 .. New_Data'Last) := Item;
         Free (B.Data);
         B.Data := New_Data;
      end if;
   end Write_Blob;

   --  Controlled type operations
   overriding procedure Initialize (Object : in out Blob) is
   begin
      Object.Data := null;
      Object.Cursor := 0;
   end Initialize;

   overriding procedure Adjust (Object : in out Blob) is
   begin
      if Object.Data /= null then
         declare
            New_Data : constant Blob_Data_Access :=
              new Ada.Streams.Stream_Element_Array'(Object.Data.all);
         begin
            Object.Data := New_Data;
         end;
      end if;
   end Adjust;

   overriding procedure Finalize (Object : in out Blob) is
      procedure Free_Data is
      begin
         if Object.Data /= null then
            Free (Object.Data);
            Object.Data := null;
         end if;
         Object.Cursor := 0;
      end Free_Data;
   begin
      Free_Data;
   end Finalize;

end Ada_Sqlite3.Blobs;
