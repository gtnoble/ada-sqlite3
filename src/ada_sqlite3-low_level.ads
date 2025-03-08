-------------------------------------------------------------------------------
-- Ada_Sqlite3.Low_Level - Low-level SQLite3 C API bindings
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Interfaces.C;
with Interfaces.C.Strings;
with System;

package Ada_Sqlite3.Low_Level is

   package C renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   --  SQLite3 database handle
   type Sqlite3 is new System.Address;
   Null_Sqlite3 : constant Sqlite3 := Sqlite3 (System.Null_Address);

   --  SQLite3 statement handle
   type Sqlite3_Stmt is new System.Address;
   Null_Sqlite3_Stmt : constant Sqlite3_Stmt := Sqlite3_Stmt (System.Null_Address);

   --  SQLite3 value handle
   type Sqlite3_Value is new System.Address;
   Null_Sqlite3_Value : constant Sqlite3_Value := Sqlite3_Value (System.Null_Address);

   --  SQLite3 context handle
   type Sqlite3_Context is new System.Address;
   Null_Sqlite3_Context : constant Sqlite3_Context := Sqlite3_Context (System.Null_Address);

   --  SQLite3 destructor type (function pointer address)
   type Destructor_Type is new System.Address;

   --  Destructor function type
   type Destructor_Callback is access procedure (Arg : System.Address);
   pragma Convention (C, Destructor_Callback);

   --  Custom destructor to free chars_ptr
   procedure Free_Chars_Ptr (Arg : System.Address);
   pragma Convention (C, Free_Chars_Ptr);
   function Free_Chars_Ptr_Address return Destructor_Type;

   --  Custom destructor to free memory
   procedure Free_Memory (Arg : System.Address);
   pragma Convention (C, Free_Memory);
   function Free_Memory_Address return Destructor_Type;

   --  Function to copy blob data
   function Copy_Blob_Data (Data : System.Address; Size : C.int) return System.Address;

   --  SQLite3 fundamental datatypes
   type Datatype is
     (SQLITE_INTEGER,
      SQLITE_FLOAT,
      SQLITE_TEXT,
      SQLITE_BLOB,
      SQLITE_NULL);
   for Datatype use
     (SQLITE_INTEGER => 1,
      SQLITE_FLOAT   => 2,
      SQLITE_TEXT    => 3,
      SQLITE_BLOB    => 4,
      SQLITE_NULL    => 5);
   pragma Convention (C, Datatype);

   --  Convert SQLite3 datatype to Ada_Sqlite3 column type
   function To_Column_Type (Value : Datatype) return Column_Type;

   --  Convert Ada_Sqlite3 column type to SQLite3 datatype
   function To_Datatype (Value : Column_Type) return Datatype;

   --  SQLite3 open database
   function Sqlite3_Open
     (Filename : CS.chars_ptr;
      DB       : access Sqlite3) return Result_Code;
   pragma Import (C, Sqlite3_Open, "sqlite3_open");

   --  SQLite3 open database with flags
   function Sqlite3_Open_V2
     (Filename : CS.chars_ptr;
      DB       : access Sqlite3;
      Flags    : C.int;
      VFS      : CS.chars_ptr) return Result_Code;
   pragma Import (C, Sqlite3_Open_V2, "sqlite3_open_v2");

   --  SQLite3 close database
   function Sqlite3_Close
     (DB : Sqlite3) return Result_Code;
   pragma Import (C, Sqlite3_Close, "sqlite3_close");

   --  SQLite3 execute SQL
   function Sqlite3_Exec
     (DB          : Sqlite3;
      SQL         : CS.chars_ptr;
      Callback    : System.Address;
      Callback_Arg : System.Address;
      ErrMsg      : access CS.chars_ptr) return Result_Code;
   pragma Import (C, Sqlite3_Exec, "sqlite3_exec");

   --  SQLite3 get last insert rowid
   function Sqlite3_Last_Insert_Rowid
     (DB : Sqlite3) return C.long;
   pragma Import (C, Sqlite3_Last_Insert_Rowid, "sqlite3_last_insert_rowid");

   --  SQLite3 get number of changes
   function Sqlite3_Changes
     (DB : Sqlite3) return C.int;
   pragma Import (C, Sqlite3_Changes, "sqlite3_changes");

   --  SQLite3 get error message
   function Sqlite3_Errmsg
     (DB : Sqlite3) return CS.chars_ptr;
   pragma Import (C, Sqlite3_Errmsg, "sqlite3_errmsg");

   --  SQLite3 get library version
   function Sqlite3_Libversion return CS.chars_ptr;
   pragma Import (C, Sqlite3_Libversion, "sqlite3_libversion");

   --  SQLite3 prepare statement
   function Sqlite3_Prepare_V2
     (DB        : Sqlite3;
      SQL       : CS.chars_ptr;
      N_Bytes   : C.int;
      Stmt      : access Sqlite3_Stmt;
      Tail      : access CS.chars_ptr) return Result_Code;
   pragma Import (C, Sqlite3_Prepare_V2, "sqlite3_prepare_v2");

   --  SQLite3 finalize statement
   function Sqlite3_Finalize
     (Stmt : Sqlite3_Stmt) return Result_Code;
   pragma Import (C, Sqlite3_Finalize, "sqlite3_finalize");

   --  SQLite3 reset statement
   function Sqlite3_Reset
     (Stmt : Sqlite3_Stmt) return Result_Code;
   pragma Import (C, Sqlite3_Reset, "sqlite3_reset");

   --  SQLite3 clear bindings
   function Sqlite3_Clear_Bindings
     (Stmt : Sqlite3_Stmt) return Result_Code;
   pragma Import (C, Sqlite3_Clear_Bindings, "sqlite3_clear_bindings");

   --  SQLite3 step statement
   function Sqlite3_Step
     (Stmt : Sqlite3_Stmt) return Result_Code;
   pragma Import (C, Sqlite3_Step, "sqlite3_step");

   --  SQLite3 bind null
   function Sqlite3_Bind_Null
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Null, "sqlite3_bind_null");

   --  SQLite3 bind integer
   function Sqlite3_Bind_Int
     (Stmt  : Sqlite3_Stmt;
      Index : C.int;
      Value : C.int) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Int, "sqlite3_bind_int");

   --  SQLite3 bind 64-bit integer
   function Sqlite3_Bind_Int64
     (Stmt  : Sqlite3_Stmt;
      Index : C.int;
      Value : C.long) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Int64, "sqlite3_bind_int64");

   --  SQLite3 bind double
   function Sqlite3_Bind_Double
     (Stmt  : Sqlite3_Stmt;
      Index : C.int;
      Value : C.double) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Double, "sqlite3_bind_double");

   --  SQLite3 bind text
   function Sqlite3_Bind_Text
     (Stmt      : Sqlite3_Stmt;
      Index     : C.int;
      Value     : CS.chars_ptr;
      N_Bytes   : C.int;
      Destructor : Destructor_Type) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Text, "sqlite3_bind_text");

   --  SQLite3 bind parameter index
   function Sqlite3_Bind_Parameter_Index
     (Stmt : Sqlite3_Stmt;
      Name : CS.chars_ptr) return C.int;
   pragma Import (C, Sqlite3_Bind_Parameter_Index, "sqlite3_bind_parameter_index");

   --  SQLite3 column count
   function Sqlite3_Column_Count
     (Stmt : Sqlite3_Stmt) return C.int;
   pragma Import (C, Sqlite3_Column_Count, "sqlite3_column_count");

   --  SQLite3 column name
   function Sqlite3_Column_Name
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return CS.chars_ptr;
   pragma Import (C, Sqlite3_Column_Name, "sqlite3_column_name");

   --  SQLite3 column type
   function Sqlite3_Column_Type
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return Datatype;
   pragma Import (C, Sqlite3_Column_Type, "sqlite3_column_type");

   --  SQLite3 column integer
   function Sqlite3_Column_Int
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return C.int;
   pragma Import (C, Sqlite3_Column_Int, "sqlite3_column_int");

   --  SQLite3 column 64-bit integer
   function Sqlite3_Column_Int64
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return C.long;
   pragma Import (C, Sqlite3_Column_Int64, "sqlite3_column_int64");

   --  SQLite3 column double
   function Sqlite3_Column_Double
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return C.double;
   pragma Import (C, Sqlite3_Column_Double, "sqlite3_column_double");

   --  SQLite3 column text
   function Sqlite3_Column_Text
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return CS.chars_ptr;
   pragma Import (C, Sqlite3_Column_Text, "sqlite3_column_text");

   --  SQLite3 column bytes
   function Sqlite3_Column_Bytes
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return C.int;
   pragma Import (C, Sqlite3_Column_Bytes, "sqlite3_column_bytes");

   --  SQLite3 column blob
   function Sqlite3_Column_Blob
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return System.Address;
   pragma Import (C, Sqlite3_Column_Blob, "sqlite3_column_blob");

   --  SQLite3 bind blob
   function Sqlite3_Bind_Blob
     (Stmt      : Sqlite3_Stmt;
      Index     : C.int;
      Value     : System.Address;
      N_Bytes   : C.int;
      Destructor : Destructor_Type) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Blob, "sqlite3_bind_blob");

end Ada_Sqlite3.Low_Level;
