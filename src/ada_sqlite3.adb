-------------------------------------------------------------------------------
-- Ada_Sqlite3 - SQLite3 bindings for Ada
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada_Sqlite3.Low_Level;
with Interfaces.C;
with Interfaces.C.Strings;

package body Ada_Sqlite3 is

   package C renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   package LL renames Ada_Sqlite3.Low_Level;
   
   use type C.int;
   use type CS.chars_ptr;
   use type System.Address;
   use type LL.Datatype;

   --  Raise an exception with the given error code and message
   procedure Raise_Error (Code : Result_Code; Message : String) is
   begin
      raise SQLite_Error with Message & " (Error code: " & Code'Image & ")";
   end Raise_Error;

   --  Check if the result code indicates an error and raise an exception if so
   procedure Check_Result (Code : Result_Code; Message : String := "") is
   begin
      if Code /= OK then
         if Message'Length > 0 then
            Raise_Error (Code, Message);
         else
            Raise_Error (Code, "SQLite3 error");
         end if;
      end if;
   end Check_Result;

   --  Get the SQLite3 library version
   function Version return String is
      Version_Ptr : constant CS.chars_ptr := LL.Sqlite3_Libversion;
   begin
      return CS.Value (Version_Ptr);
   end Version;

   --  Database implementation

   --  Open a new database connection
   procedure Open
     (DB       : in out Database;
      Filename : String;
      Flags    : Open_Flag := OPEN_READWRITE or OPEN_CREATE)
   is
      C_Filename : CS.chars_ptr := CS.New_String (Filename);
      DB_Handle  : aliased LL.Sqlite3;
      Result     : Result_Code;
   begin
      --  Close the database if it's already open
      if DB.Is_Open then
         Close (DB);
      end if;

      --  Open the database
      Result := LL.Sqlite3_Open_V2
        (Filename => C_Filename,
         DB       => DB_Handle'Access,
         Flags    => C.int (Flags),
         VFS      => CS.Null_Ptr);

      --  Free the filename string
      CS.Free (C_Filename);

      --  Check for errors
      if Result /= OK then
         Raise_Error (Result, "Failed to open database: " & Filename);
      end if;

      --  Set the database handle
      DB.Handle := System.Address (DB_Handle);
      DB.Is_Open_Flag := True;
   end Open;

   --  Close a database connection
   procedure Close (DB : in out Database) is
      Result : Result_Code;
   begin
      if DB.Is_Open then
         Result := LL.Sqlite3_Close (LL.Sqlite3 (DB.Handle));
         if Result /= OK then
            Raise_Error (Result, "Failed to close database");
         end if;
         DB.Handle := System.Null_Address;
         DB.Is_Open_Flag := False;
      end if;
   end Close;

   --  Check if the database connection is open
   function Is_Open (DB : Database) return Boolean is
   begin
      return DB.Is_Open_Flag;
   end Is_Open;

   --  Execute a simple SQL statement that doesn't return results
   procedure Execute
     (DB  : in out Database;
      SQL : String)
   is
      C_SQL    : CS.chars_ptr := CS.New_String (SQL);
      Err_Msg  : aliased CS.chars_ptr := CS.Null_Ptr;
      Result   : Result_Code;
   begin
      if not DB.Is_Open then
         raise SQLite_Error with "Database is not open";
      end if;

      Result := LL.Sqlite3_Exec
        (DB          => LL.Sqlite3 (DB.Handle),
         SQL         => C_SQL,
         Callback    => System.Null_Address,
         Callback_Arg => System.Null_Address,
         ErrMsg      => Err_Msg'Access);

      --  Free the SQL string
      CS.Free (C_SQL);

      --  Check for errors
      if Result /= OK then
         declare
            Error_Message : constant String := CS.Value (Err_Msg);
         begin
            CS.Free (Err_Msg);
            Raise_Error (Result, "SQL execution error: " & Error_Message);
         end;
      elsif Err_Msg /= CS.Null_Ptr then
         --  Free the error message if it was allocated but no error occurred
         CS.Free (Err_Msg);
      end if;
   end Execute;

   --  Get the last inserted row ID
   function Last_Insert_Row_ID (DB : Database) return Long_Integer is
   begin
      if not DB.Is_Open then
         raise SQLite_Error with "Database is not open";
      end if;

      return Long_Integer (LL.Sqlite3_Last_Insert_Rowid (LL.Sqlite3 (DB.Handle)));
   end Last_Insert_Row_ID;

   --  Get the number of rows changed by the last statement
   function Changes (DB : Database) return Integer is
   begin
      if not DB.Is_Open then
         raise SQLite_Error with "Database is not open";
      end if;

      return Integer (LL.Sqlite3_Changes (LL.Sqlite3 (DB.Handle)));
   end Changes;

   --  Finalize a database connection
   overriding procedure Finalize (DB : in out Database) is
   begin
      Close (DB);
   end Finalize;

   --  Statement implementation

   --  Prepare a SQL statement
   procedure Prepare
     (Stmt : in out Statement'Class;
      DB   : in out Database;
      SQL  : String)
   is
      C_SQL    : CS.chars_ptr := CS.New_String (SQL);
      Stmt_Handle : aliased LL.Sqlite3_Stmt;
      Tail     : aliased CS.chars_ptr := CS.Null_Ptr;
      Result   : Result_Code;
   begin
      if not DB.Is_Open then
         raise SQLite_Error with "Database is not open";
      end if;

      --  Finalize the statement if it's already prepared
      if Stmt.Handle /= System.Null_Address then
         Finalize_Statement (Stmt);
      end if;

      Result := LL.Sqlite3_Prepare_V2
        (DB        => LL.Sqlite3 (DB.Handle),
         SQL       => C_SQL,
         N_Bytes   => C.int (SQL'Length),
         Stmt      => Stmt_Handle'Access,
         Tail      => Tail'Access);

      --  Free the SQL string
      CS.Free (C_SQL);

      --  Check for errors
      if Result /= OK then
         Raise_Error (Result, "Failed to prepare statement: " & SQL);
      end if;

      --  Set the statement handle
      Stmt.Handle := System.Address (Stmt_Handle);
      Stmt.DB := DB'Unchecked_Access;
   end Prepare;

   --  Finalize a prepared statement
   procedure Finalize_Statement (Stmt : in out Statement'Class) is
   begin
      --  Only finalize if the statement handle is not null
      if Stmt.Handle /= System.Null_Address then
         declare
            Result : Result_Code;
            Temp_Handle : constant LL.Sqlite3_Stmt := LL.Sqlite3_Stmt (Stmt.Handle);
         begin
            --  Set the handle to null first to prevent double finalization
            Stmt.Handle := System.Null_Address;
            Stmt.DB := null;
            
            --  Finalize the statement
            begin
               Result := LL.Sqlite3_Finalize (Temp_Handle);
               if Result /= OK and Result /= MISUSE then
                  Raise_Error (Result, "Failed to finalize statement");
               end if;
            exception
               when others =>
                  --  Ignore any exceptions during finalization
                  null;
            end;
         end;
      end if;
   end Finalize_Statement;

   --  Finalize a statement (called by Ada.Finalization)
   overriding procedure Finalize (Stmt : in out Statement) is
   begin
      Finalize_Statement (Stmt);
   end Finalize;

   --  Reset a prepared statement
   procedure Reset (Stmt : in out Statement) is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Reset (LL.Sqlite3_Stmt (Stmt.Handle));
      if Result /= OK then
         Raise_Error (Result, "Failed to reset statement");
      end if;
   end Reset;

   --  Clear all bindings on a prepared statement
   procedure Clear_Bindings (Stmt : in out Statement) is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Clear_Bindings (LL.Sqlite3_Stmt (Stmt.Handle));
      if Result /= OK then
         Raise_Error (Result, "Failed to clear bindings");
      end if;
   end Clear_Bindings;

   --  Execute a prepared statement and return the result code
   function Step (Stmt : in out Statement) return Result_Code is
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      return LL.Sqlite3_Step (LL.Sqlite3_Stmt (Stmt.Handle));
   end Step;

   --  Execute a prepared statement and check for errors
   procedure Step (Stmt : in out Statement) is
      Result : constant Result_Code := Step (Stmt);
   begin
      if Result /= ROW and Result /= DONE then
         Raise_Error (Result, "Error executing statement");
      end if;
   end Step;

   --  Bind a NULL value to a parameter
   procedure Bind_Null
     (Stmt  : in out Statement;
      Index : Positive)
   is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Bind_Null
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      if Result /= OK then
         Raise_Error (Result, "Failed to bind NULL value");
      end if;
   end Bind_Null;

   --  Bind an Integer value to a parameter
   procedure Bind_Int
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Integer)
   is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Bind_Int
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index),
         Value => C.int (Value));

      if Result /= OK then
         Raise_Error (Result, "Failed to bind integer value");
      end if;
   end Bind_Int;

   --  Bind a Long_Integer value to a parameter
   procedure Bind_Int64
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Long_Integer)
   is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Bind_Int64
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index),
         Value => C.long (Value));

      if Result /= OK then
         Raise_Error (Result, "Failed to bind 64-bit integer value");
      end if;
   end Bind_Int64;

   --  Bind a Float value to a parameter
   procedure Bind_Double
     (Stmt  : in out Statement;
      Index : Positive;
      Value : Float)
   is
      Result : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Bind_Double
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index),
         Value => C.double (Value));

      if Result /= OK then
         Raise_Error (Result, "Failed to bind float value");
      end if;
   end Bind_Double;

   --  Bind a String value to a parameter
   procedure Bind_Text
     (Stmt  : in out Statement;
      Index : Positive;
      Value : String)
   is
      C_Value : CS.chars_ptr := CS.New_String (Value);
      Result  : Result_Code;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Result := LL.Sqlite3_Bind_Text
        (Stmt      => LL.Sqlite3_Stmt (Stmt.Handle),
         Index     => C.int (Index),
         Value     => C_Value,
         N_Bytes   => C.int (Value'Length),
         Destructor => LL.SQLITE_TRANSIENT);

      --  Free the string
      CS.Free (C_Value);

      if Result /= OK then
         Raise_Error (Result, "Failed to bind text value");
      end if;
   end Bind_Text;

   --  Bind a parameter by name
   procedure Bind_Parameter_Index
     (Stmt      : Statement;
      Name      : String;
      Parameter : out Positive)
   is
      C_Name : CS.chars_ptr := CS.New_String (Name);
      Index  : C.int;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Index := LL.Sqlite3_Bind_Parameter_Index
        (Stmt => LL.Sqlite3_Stmt (Stmt.Handle),
         Name => C_Name);

      --  Free the string
      CS.Free (C_Name);

      if Index <= 0 then
         raise SQLite_Error with "Parameter not found: " & Name;
      end if;

      Parameter := Positive (Index);
   end Bind_Parameter_Index;

   --  Get the number of columns in the result set
   function Column_Count (Stmt : Statement) return Natural is
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      return Natural (LL.Sqlite3_Column_Count (LL.Sqlite3_Stmt (Stmt.Handle)));
   end Column_Count;

   --  Get the name of a column
   function Column_Name
     (Stmt  : Statement;
      Index : Natural) return String
   is
      Name_Ptr : CS.chars_ptr;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Name_Ptr := LL.Sqlite3_Column_Name
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      if Name_Ptr = CS.Null_Ptr then
         raise SQLite_Error with "Failed to get column name";
      end if;

      return CS.Value (Name_Ptr);
   end Column_Name;

   --  Get the type of a column
   function Get_Column_Type
     (Stmt  : Statement;
      Index : Natural) return Column_Type
   is
      Datatype : LL.Datatype;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Datatype := LL.Sqlite3_Column_Type
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      return LL.To_Column_Type (Datatype);
   end Get_Column_Type;

   --  Get an Integer value from a column
   function Column_Int
     (Stmt  : Statement;
      Index : Natural) return Integer
   is
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      return Integer (LL.Sqlite3_Column_Int
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index)));
   end Column_Int;

   --  Get a Long_Integer value from a column
   function Column_Int64
     (Stmt  : Statement;
      Index : Natural) return Long_Integer
   is
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      return Long_Integer (LL.Sqlite3_Column_Int64
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index)));
   end Column_Int64;

   --  Get a Float value from a column
   function Column_Double
     (Stmt  : Statement;
      Index : Natural) return Float
   is
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      return Float (LL.Sqlite3_Column_Double
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index)));
   end Column_Double;

   --  Get a String value from a column
   function Column_Text
     (Stmt  : Statement;
      Index : Natural) return String
   is
      Text_Ptr : CS.chars_ptr;
   begin
      if Stmt.Handle = System.Null_Address then
         raise SQLite_Error with "Statement is not prepared";
      end if;

      Text_Ptr := LL.Sqlite3_Column_Text
        (Stmt  => LL.Sqlite3_Stmt (Stmt.Handle),
         Index => C.int (Index));

      if Text_Ptr = CS.Null_Ptr then
         return "";
      end if;

      return CS.Value (Text_Ptr);
   end Column_Text;

   --  Check if a column value is NULL
   function Column_Is_Null
     (Stmt  : Statement;
      Index : Natural) return Boolean
   is
   begin
      return Get_Column_Type (Stmt, Index) = Null_Type;
   end Column_Is_Null;

end Ada_Sqlite3;
