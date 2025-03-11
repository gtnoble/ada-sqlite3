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
with Ada.Unchecked_Conversion;

package Ada_Sqlite3.Low_Level is
   package C renames Interfaces.C;
   package CS renames Interfaces.C.Strings;

   -- Use types from Ada_Sqlite3
   subtype Result_Code is Ada_Sqlite3.Result_Code;
   subtype Column_Type is Ada_Sqlite3.Column_Type;
   
   -- Common result codes
   SQLITE_OK          : constant Result_Code := Ada_Sqlite3.OK;
   SQLITE_ERROR       : constant Result_Code := Ada_Sqlite3.ERROR;
   SQLITE_INTERNAL    : constant Result_Code := Ada_Sqlite3.INTERNAL;
   SQLITE_PERM        : constant Result_Code := Ada_Sqlite3.PERM;
   SQLITE_ABORT       : constant Result_Code := Ada_Sqlite3.ABORT_CODE;
   SQLITE_BUSY        : constant Result_Code := Ada_Sqlite3.BUSY;
   SQLITE_LOCKED      : constant Result_Code := Ada_Sqlite3.LOCKED;
   SQLITE_NOMEM       : constant Result_Code := Ada_Sqlite3.NOMEM;
   SQLITE_READONLY    : constant Result_Code := Ada_Sqlite3.READONLY;
   SQLITE_INTERRUPT   : constant Result_Code := Ada_Sqlite3.INTERRUPT;
   SQLITE_IOERR       : constant Result_Code := Ada_Sqlite3.IOERR;
   SQLITE_CORRUPT     : constant Result_Code := Ada_Sqlite3.CORRUPT;
   SQLITE_NOTFOUND    : constant Result_Code := Ada_Sqlite3.NOTFOUND;
   SQLITE_FULL        : constant Result_Code := Ada_Sqlite3.FULL;
   SQLITE_CANTOPEN    : constant Result_Code := Ada_Sqlite3.CANTOPEN;
   SQLITE_PROTOCOL    : constant Result_Code := Ada_Sqlite3.PROTOCOL;
   SQLITE_EMPTY       : constant Result_Code := Ada_Sqlite3.EMPTY;
   SQLITE_SCHEMA      : constant Result_Code := Ada_Sqlite3.SCHEMA;
   SQLITE_TOOBIG      : constant Result_Code := Ada_Sqlite3.TOOBIG;
   SQLITE_CONSTRAINT  : constant Result_Code := Ada_Sqlite3.CONSTRAINT;
   SQLITE_MISMATCH    : constant Result_Code := Ada_Sqlite3.MISMATCH;
   SQLITE_MISUSE      : constant Result_Code := Ada_Sqlite3.MISUSE;
   SQLITE_NOLFS       : constant Result_Code := Ada_Sqlite3.NOLFS;
   SQLITE_AUTH        : constant Result_Code := Ada_Sqlite3.AUTH;
   SQLITE_FORMAT      : constant Result_Code := Ada_Sqlite3.FORMAT;
   SQLITE_RANGE       : constant Result_Code := Ada_Sqlite3.RANGE_ERROR;
   SQLITE_NOTADB      : constant Result_Code := Ada_Sqlite3.NOTADB;
   SQLITE_NOTICE      : constant Result_Code := Ada_Sqlite3.NOTICE;
   SQLITE_WARNING     : constant Result_Code := Ada_Sqlite3.WARNING;
   SQLITE_ROW         : constant Result_Code := Ada_Sqlite3.ROW;
   SQLITE_DONE        : constant Result_Code := Ada_Sqlite3.DONE;

   -- Use SQLite3 handles from Ada_Sqlite3
   subtype Sqlite3 is Ada_Sqlite3.Sqlite3;
   subtype Sqlite3_Stmt is Ada_Sqlite3.Sqlite3_Stmt;
   subtype Sqlite3_Value is Ada_Sqlite3.Sqlite3_Value;
   subtype Sqlite3_Context is Ada_Sqlite3.Sqlite3_Context;
   
   -- Null constants with proper conversions
   function To_Sqlite3 is new Ada.Unchecked_Conversion(System.Address, Sqlite3);
   function To_Sqlite3_Stmt is new Ada.Unchecked_Conversion(System.Address, Sqlite3_Stmt);
   function To_Sqlite3_Value is new Ada.Unchecked_Conversion(System.Address, Sqlite3_Value);
   function To_Sqlite3_Context is new Ada.Unchecked_Conversion(System.Address, Sqlite3_Context);

   Null_Sqlite3 : constant Sqlite3 := To_Sqlite3(System.Null_Address);
   Null_Sqlite3_Stmt : constant Sqlite3_Stmt := To_Sqlite3_Stmt(System.Null_Address);
   Null_Sqlite3_Value : constant Sqlite3_Value := To_Sqlite3_Value(System.Null_Address);
   Null_Sqlite3_Context : constant Sqlite3_Context := To_Sqlite3_Context(System.Null_Address);

   --  SQLite3 destructor type (function pointer address)
   type Destructor_Type is new System.Address;
   
   -- Special destructor constant for transient text/blob data
   SQLITE_TRANSIENT : constant Destructor_Type := 
      Destructor_Type(System'To_Address(-1));

   -- Function flags
   SQLITE_UTF8 : constant C.int := 1;
   SQLITE_DETERMINISTIC : constant C.int := 16#800#;

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

   -- Value functions
   function Sqlite3_Value_Type (Value : Sqlite3_Value) return Datatype;
   pragma Import (C, Sqlite3_Value_Type, "sqlite3_value_type");

   function Sqlite3_Value_Int (Value : Sqlite3_Value) return C.int;
   pragma Import (C, Sqlite3_Value_Int, "sqlite3_value_int");

   function Sqlite3_Value_Int64 (Value : Sqlite3_Value) return C.long;
   pragma Import (C, Sqlite3_Value_Int64, "sqlite3_value_int64");

   function Sqlite3_Value_Double (Value : Sqlite3_Value) return C.double;
   pragma Import (C, Sqlite3_Value_Double, "sqlite3_value_double");

   function Sqlite3_Value_Text (Value : Sqlite3_Value) return CS.chars_ptr;
   pragma Import (C, Sqlite3_Value_Text, "sqlite3_value_text");

   function Sqlite3_Value_Text16 (Value : Sqlite3_Value) return System.Address;
   pragma Import (C, Sqlite3_Value_Text16, "sqlite3_value_text16");

   function Sqlite3_Value_Bytes16 (Value : Sqlite3_Value) return C.int;
   pragma Import (C, Sqlite3_Value_Bytes16, "sqlite3_value_bytes16");

   function Sqlite3_Value_Blob (Value : Sqlite3_Value) return System.Address;
   pragma Import (C, Sqlite3_Value_Blob, "sqlite3_value_blob");

   function Sqlite3_Value_Bytes (Value : Sqlite3_Value) return C.int;
   pragma Import (C, Sqlite3_Value_Bytes, "sqlite3_value_bytes");

   -- Result functions
   procedure Sqlite3_Result_Null (Context : Sqlite3_Context);
   pragma Import (C, Sqlite3_Result_Null, "sqlite3_result_null");

   procedure Sqlite3_Result_Int (Context : Sqlite3_Context; Value : C.int);
   pragma Import (C, Sqlite3_Result_Int, "sqlite3_result_int");

   procedure Sqlite3_Result_Int64 (Context : Sqlite3_Context; Value : C.long_long);
   pragma Import (C, Sqlite3_Result_Int64, "sqlite3_result_int64");

   procedure Sqlite3_Result_Double (Context : Sqlite3_Context; Value : C.double);
   pragma Import (C, Sqlite3_Result_Double, "sqlite3_result_double");

   procedure Sqlite3_Result_Text (
      Context    : Sqlite3_Context;
      Value      : CS.chars_ptr;
      N_Bytes    : C.int;
      Destructor : Destructor_Type);
   pragma Import (C, Sqlite3_Result_Text, "sqlite3_result_text");

   procedure Sqlite3_Result_Text16 (
      Context    : Sqlite3_Context;
      Value      : System.Address;
      N_Bytes    : C.int;
      Destructor : Destructor_Type);
   pragma Import (C, Sqlite3_Result_Text16, "sqlite3_result_text16");

   procedure Sqlite3_Result_Text16be (
      Context    : Sqlite3_Context;
      Value      : System.Address;
      N_Bytes    : C.int;
      Destructor : Destructor_Type);
   pragma Import (C, Sqlite3_Result_Text16be, "sqlite3_result_text16be");

   procedure Sqlite3_Result_Text16le (
      Context    : Sqlite3_Context;
      Value      : System.Address;
      N_Bytes    : C.int;
      Destructor : Destructor_Type);
   pragma Import (C, Sqlite3_Result_Text16le, "sqlite3_result_text16le");

   procedure Sqlite3_Result_Blob (
      Context    : Sqlite3_Context;
      Value      : System.Address;
      N_Bytes    : C.int;
      Destructor : Destructor_Type);
   pragma Import (C, Sqlite3_Result_Blob, "sqlite3_result_blob");

   procedure Sqlite3_Result_Zeroblob (Context : Sqlite3_Context; Size : C.int);
   pragma Import (C, Sqlite3_Result_Zeroblob, "sqlite3_result_zeroblob");

   procedure Sqlite3_Result_Error (
      Context    : Sqlite3_Context;
      Message    : CS.chars_ptr;
      N_Bytes    : C.int);
   pragma Import (C, Sqlite3_Result_Error, "sqlite3_result_error");

   procedure Sqlite3_Result_Error_Code (Context : Sqlite3_Context; Code : C.int);
   pragma Import (C, Sqlite3_Result_Error_Code, "sqlite3_result_error_code");

   procedure Sqlite3_Result_Error_Nomem (Context : Sqlite3_Context);
   pragma Import (C, Sqlite3_Result_Error_Nomem, "sqlite3_result_error_nomem");

   procedure Sqlite3_Result_Error_Too_Big (Context : Sqlite3_Context);
   pragma Import (C, Sqlite3_Result_Error_Too_Big, "sqlite3_result_error_toobig");

   -- Function callback types
   type Sqlite3_Value_Array is array (C.size_t range <>) of Sqlite3_Value
     with Convention => C;

   type Sqlite3_Func_Callback is access procedure (
      Context : Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Sqlite3_Value_Array);
   pragma Convention (C, Sqlite3_Func_Callback);

   type Sqlite3_Step_Callback is access procedure (
      Context : Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Sqlite3_Value_Array);
   pragma Convention (C, Sqlite3_Step_Callback);

   type Sqlite3_Final_Callback is access procedure (
      Context : Sqlite3_Context);
   pragma Convention (C, Sqlite3_Final_Callback);

   type Sqlite3_Destroy_Callback is access procedure (
      User_Data : System.Address);
   pragma Convention (C, Sqlite3_Destroy_Callback);

   -- Window function callback type
   type Sqlite3_Window_Callback is access procedure (
      Context : Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Sqlite3_Value_Array);
   pragma Convention (C, Sqlite3_Window_Callback);

   -- Create function API
   function Sqlite3_Create_Function (
      DB           : Sqlite3;
      Function_Name : CS.chars_ptr;
      N_Arg        : C.int;
      Encoding     : C.int;
      User_Data    : System.Address;
      Func         : Sqlite3_Func_Callback;
      Step         : Sqlite3_Step_Callback;
      Final        : Sqlite3_Final_Callback) return Result_Code;
   pragma Import (C, Sqlite3_Create_Function, "sqlite3_create_function");

   function Sqlite3_Create_Function_V2 (
      DB           : Sqlite3;
      Function_Name : CS.chars_ptr;
      N_Arg        : C.int;
      Encoding     : C.int;
      User_Data    : System.Address;
      Func         : Sqlite3_Func_Callback;
      Step         : Sqlite3_Step_Callback;
      Final        : Sqlite3_Final_Callback;
      Destroy      : Sqlite3_Destroy_Callback) return Result_Code;
   pragma Import (C, Sqlite3_Create_Function_V2, "sqlite3_create_function_v2");

   function Sqlite3_Create_Window_Function (
      DB           : Sqlite3;
      Function_Name : CS.chars_ptr;
      N_Arg        : C.int;
      Encoding     : C.int;
      User_Data    : System.Address;
      Step         : Sqlite3_Step_Callback;
      Final        : Sqlite3_Final_Callback;
      Value        : Sqlite3_Window_Callback;
      Inverse      : Sqlite3_Step_Callback;
      Destroy      : Sqlite3_Destroy_Callback) return Result_Code;
   pragma Import (C, Sqlite3_Create_Window_Function, "sqlite3_create_window_function");

   -- Context access
   function Sqlite3_User_Data (Context : Sqlite3_Context) return System.Address;
   pragma Import (C, Sqlite3_User_Data, "sqlite3_user_data");

   -- C binding for aggregate context
   function Sqlite3_Aggregate_Context (
      Context : Sqlite3_Context;
      N_Bytes : C.int) return System.Address;
   pragma Import (C, Sqlite3_Aggregate_Context, "sqlite3_aggregate_context");

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

   -- SQLite3 bind UTF-16 text
   function Sqlite3_Bind_Text16
     (Stmt      : Sqlite3_Stmt;
      Index     : C.int;
      Value     : System.Address;
      N_Bytes   : C.int;
      Destructor : Destructor_Type) return Result_Code;
   pragma Import (C, Sqlite3_Bind_Text16, "sqlite3_bind_text16");

   --  SQLite3 bind parameter index
   function Sqlite3_Bind_Parameter_Index
     (Stmt : Sqlite3_Stmt;
      Name : CS.chars_ptr) return C.int;
   pragma Import (C, Sqlite3_Bind_Parameter_Index, "sqlite3_bind_parameter_index");

   --  Get number of parameters in prepared statement
   function Sqlite3_Bind_Parameter_Count
     (Stmt : Sqlite3_Stmt) return C.int;
   pragma Import (C, Sqlite3_Bind_Parameter_Count, "sqlite3_bind_parameter_count");

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

   --  SQLite3 column text UTF-16
   function Sqlite3_Column_Text16
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return System.Address;
   pragma Import (C, Sqlite3_Column_Text16, "sqlite3_column_text16");

   --  SQLite3 column bytes for UTF-16 text
   function Sqlite3_Column_Bytes16
     (Stmt  : Sqlite3_Stmt;
      Index : C.int) return C.int;
   pragma Import (C, Sqlite3_Column_Bytes16, "sqlite3_column_bytes16");

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
