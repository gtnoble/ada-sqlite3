-------------------------------------------------------------------------------
-- Ada_Sqlite3 - SQLite3 bindings for Ada
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Finalization;
with System;

package Ada_Sqlite3 is
   --  SQLite3 Error Codes
   type Result_Code is new Integer;

   --  Common result codes
   OK          : constant Result_Code := 0;   --  Successful result
   ERROR       : constant Result_Code := 1;   --  Generic error
   INTERNAL    : constant Result_Code := 2;   --  Internal logic error in SQLite
   PERM        : constant Result_Code := 3;   --  Access permission denied
   ABORT_CODE  : constant Result_Code := 4;   --  Callback routine requested an abort
   BUSY        : constant Result_Code := 5;   --  The database file is locked
   LOCKED      : constant Result_Code := 6;   --  A table in the database is locked
   NOMEM       : constant Result_Code := 7;   --  A malloc() failed
   READONLY    : constant Result_Code := 8;   --  Attempt to write a readonly database
   INTERRUPT   : constant Result_Code := 9;   --  Operation terminated by sqlite3_interrupt()
   IOERR       : constant Result_Code := 10;  --  Some kind of disk I/O error occurred
   CORRUPT     : constant Result_Code := 11;  --  The database disk image is malformed
   NOTFOUND    : constant Result_Code := 12;  --  Unknown opcode in sqlite3_file_control()
   FULL        : constant Result_Code := 13;  --  Insertion failed because database is full
   CANTOPEN    : constant Result_Code := 14;  --  Unable to open the database file
   PROTOCOL    : constant Result_Code := 15;  --  Database lock protocol error
   EMPTY       : constant Result_Code := 16;  --  Internal use only
   SCHEMA      : constant Result_Code := 17;  --  The database schema changed
   TOOBIG      : constant Result_Code := 18;  --  String or BLOB exceeds size limit
   CONSTRAINT  : constant Result_Code := 19;  --  Abort due to constraint violation
   MISMATCH    : constant Result_Code := 20;  --  Data type mismatch
   MISUSE      : constant Result_Code := 21;  --  Library used incorrectly
   NOLFS       : constant Result_Code := 22;  --  Uses OS features not supported on host
   AUTH        : constant Result_Code := 23;  --  Authorization denied
   FORMAT      : constant Result_Code := 24;  --  Not used
   RANGE_ERROR : constant Result_Code := 25;  --  2nd parameter to sqlite3_bind out of range
   NOTADB      : constant Result_Code := 26;  --  File opened that is not a database file
   NOTICE      : constant Result_Code := 27;  --  Notifications from sqlite3_log()
   WARNING     : constant Result_Code := 28;  --  Warnings from sqlite3_log()
   ROW         : constant Result_Code := 100; --  sqlite3_step() has another row ready
   DONE        : constant Result_Code := 101; --  sqlite3_step() has finished executing

   --  SQLite3 Error Exception
   SQLite_Error : exception;

   --  Raise an exception with the given error code and message
   procedure Raise_Error (Code : Result_Code; Message : String);

   --  Check if the result code indicates an error and raise an exception if so
   procedure Check_Result (Code : Result_Code; Message : String := "");

   --  SQLite3 Column Types
   type Column_Type is
     (Integer_Type,
      Float_Type,
      Text_Type,
      Blob_Type,
      Null_Type);

   --  SQLite3 Open Flags
   type Open_Flag is mod 2**32;
   OPEN_READONLY   : constant Open_Flag := 16#00000001#;
   OPEN_READWRITE  : constant Open_Flag := 16#00000002#;
   OPEN_CREATE     : constant Open_Flag := 16#00000004#;
   OPEN_URI        : constant Open_Flag := 16#00000040#;
   OPEN_MEMORY     : constant Open_Flag := 16#00000080#;
   OPEN_NOMUTEX    : constant Open_Flag := 16#00008000#;
   OPEN_FULLMUTEX  : constant Open_Flag := 16#00010000#;
   OPEN_SHAREDCACHE : constant Open_Flag := 16#00020000#;
   OPEN_PRIVATECACHE : constant Open_Flag := 16#00040000#;
   OPEN_NOFOLLOW   : constant Open_Flag := 16#01000000#;

   --  Database connection type
   type Database is tagged limited private;

   --  Open a new database connection
   function Open
     (Filename : String;
      Flags    : Open_Flag := OPEN_READWRITE or OPEN_CREATE) return Database;

   --  Check if the database connection is open
   function Is_Open (DB : Database) return Boolean;

   --  Execute a simple SQL statement that doesn't return results
   procedure Execute
     (DB  : in out Database;
      SQL : String);

   --  Get the last inserted row ID
   function Last_Insert_Row_ID (DB : Database) return Long_Integer;

   -- Forward declarations for SQLite3 handles
   type Sqlite3 is private;
   type Sqlite3_Stmt is private;
   type Sqlite3_Value is private;
   type Sqlite3_Context is private;

   --  Get the number of rows changed by the last statement
   function Changes (DB : Database) return Integer;

   --  Get the SQLite3 library version
   function Version return String;

   --  Prepared statement type
   type Statement is tagged limited private;

   --  Prepare a SQL statement
   function Prepare
     (DB   : in out Database'Class;
      SQL  : String) return Statement;

   --  Reset a prepared statement
   procedure Reset (Stmt : in out Statement);

   --  Clear all bindings on a prepared statement
   procedure Clear_Bindings (Stmt : in out Statement);

   --  Execute a prepared statement and return the result code
   function Step (Stmt : in out Statement) return Result_Code;

   --  Execute a prepared statement and check for errors
   procedure Step (Stmt : in out Statement);

   --  Bind a NULL value to a parameter
   procedure Bind_Null
     (Stmt  : in out Statement;
      Index : Positive);

   procedure Bind_Null
     (Stmt  : in out Statement;
      Name  : String);

   --  Bind an Integer value to a parameter
   procedure Bind_Int
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Integer);

   procedure Bind_Int
     (Stmt  : in out Statement;
      Name  : String;
      Value : Integer);

   --  Bind a Long_Integer value to a parameter
   procedure Bind_Int64
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Long_Integer);

   procedure Bind_Int64
     (Stmt  : in out Statement;
      Name  : String;
      Value : Long_Integer);

   --  Bind a Float value to a parameter
   procedure Bind_Double
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Float);

   procedure Bind_Double
     (Stmt  : in out Statement;
      Name  : String;
      Value : Float);

   --  Bind a String value to a parameter
   procedure Bind_Text
     (Stmt  : in out Statement;
      Index : Positive;
      Value : String);

   procedure Bind_Text
     (Stmt  : in out Statement;
      Name  : String;
      Value : String);

   --  Bind a Wide_String value to a parameter as UTF-16 text
   procedure Bind_Text_UTF16
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Wide_String);

   procedure Bind_Text_UTF16
     (Stmt  : in out Statement;
      Name  : String;
      Value : Wide_String);

   --  Bind a parameter by name
   procedure Bind_Parameter_Index
     (Stmt      : Statement;
      Name      : String;
      Parameter : out Positive);

   --  Get the number of columns in the result set
   function Column_Count (Stmt : Statement) return Natural;

   --  Get the name of a column
   function Column_Name
     (Stmt  : Statement;
      Index : Natural) return String;

   --  Get the type of a column in the result set
   function Get_Column_Type
     (Stmt  : Statement;
      Index : Natural) return Column_Type;

   --  Get an Integer value from a column
   function Column_Int
     (Stmt  : Statement;
      Index : Natural) return Integer;

   --  Get a Long_Integer value from a column
   function Column_Int64
     (Stmt  : Statement;
      Index : Natural) return Long_Integer;

   --  Get a Float value from a column
   function Column_Double
     (Stmt  : Statement;
      Index : Natural) return Float;

   --  Get a String value from a column
   function Column_Text
     (Stmt  : Statement;
      Index : Natural) return String;

   --  Get a Wide_String value from a column as UTF-16 text
   function Column_Text_UTF16
     (Stmt  : Statement;
      Index : Natural) return Wide_String;

   --  Check if a column value is NULL
   function Column_Is_Null
     (Stmt  : Statement;
      Index : Natural) return Boolean;

private
   -- SQLite3 handles
   type Sqlite3 is new System.Address;
   type Sqlite3_Stmt is new System.Address;
   type Sqlite3_Value is new System.Address;
   type Sqlite3_Context is new System.Address;
   
   type Database_Access is access all Database'Class;
   type Statement_Access is access all Statement'Class;

   --  Forward declaration for Statement_List
   type Statement_List;
   type Statement_List_Access is access Statement_List;

   --  List of statements associated with a database
   type Statement_List is record
      Stmt : Statement_Access;
      Next : Statement_List_Access;
   end record;

   type Database is new Ada.Finalization.Limited_Controlled with record
      Handle       : System.Address := System.Null_Address;
      Is_Open_Flag : Boolean := False;
      Statements   : Statement_List_Access := null;  --  List of associated statements
      Finalizing   : Boolean := False;  --  Flag to indicate database is being finalized
   end record;

   --  Make Database a reference-counted type
   type Database_Ref is access all Database;
   for Database_Ref'Storage_Size use 0;  -- No heap allocation

   --  Close a database connection
   procedure Close (DB : in out Database);

   overriding procedure Finalize (DB : in out Database);

   type Statement is new Ada.Finalization.Limited_Controlled with record
      Handle      : System.Address := System.Null_Address;
      DB          : Database_Access := null;
      List_Node   : Statement_List_Access := null;  --  Reference to list node containing this statement
      Finalized   : Boolean := False;  --  Flag to track whether the statement has been finalized
   end record;

   --  Make Statement a reference-counted type
   type Statement_Ref is access all Statement;
   for Statement_Ref'Storage_Size use 0;  -- No heap allocation
    
   --  Finalize a prepared statement (release resources)
   procedure Finalize_Statement (Stmt : in out Statement'Class);

   --  Register a statement with its database
   procedure Register_Statement (DB : in out Database; Stmt : in out Statement'Class);

   --  Unregister a statement from its database
   procedure Unregister_Statement (Stmt : in out Statement'Class);

   overriding procedure Finalize (Stmt : in out Statement);

end Ada_Sqlite3;
