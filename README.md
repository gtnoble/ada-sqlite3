# Ada_Sqlite3

SQLite3 bindings for Ada.

## Overview

Ada_Sqlite3 provides a convenient and type-safe Ada interface to the SQLite3 database library. It offers both low-level bindings to the C API and a higher-level, more Ada-idiomatic interface.

## Features

- Complete bindings to the SQLite3 C API
- High-level Ada interface with strong typing
- Resource management using controlled types
- Exception-based error handling
- Support for prepared statements
- Support for all SQLite3 data types
- Thread-safe operations
- Generic function support (scalar, aggregate, window functions)
- BLOB operations with stream interface
- String and Wide_String text support
- Named parameter binding
- Comprehensive error handling and reporting

## Requirements

- GNAT Ada compiler
- SQLite3 development libraries
- Alire package manager (recommended)

## Installation

### Using Alire (recommended)

```bash
alr with ada_sqlite3
```

### Manual Installation

1. Ensure SQLite3 development libraries are installed on your system
2. Clone this repository
3. Build the library:

```bash
gprbuild -P ada_sqlite3.gpr
```

## Usage

### Basic Example

```ada
with Ada.Text_IO;
with Ada_Sqlite3;

procedure Example is
   use Ada.Text_IO;
   use Ada_Sqlite3;

   DB : Database;
begin
   -- Open a database
   Open(DB, "test.db");

   -- Execute a simple SQL statement
   Execute(DB, "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, name TEXT)");
   Execute(DB, "INSERT INTO test (name) VALUES ('Example')");

   -- Use a prepared statement
   declare
      Stmt : Statement;
   begin
      Prepare(Stmt, DB, "SELECT id, name FROM test");
      
      -- Fetch rows
      while Step(Stmt) = ROW loop
         Put_Line("ID: " & Column_Int(Stmt, 0)'Image & 
                  ", Name: " & Column_Text(Stmt, 1));
      end loop;
      
   end;

end Example;
```

### Working with Prepared Statements

```ada
with Ada_Sqlite3;

procedure Statements_Example is
   use Ada_Sqlite3;

   DB : Database;
   Stmt : Statement;
begin
   Open(DB, "test.db");
   
   -- Prepare a statement with parameters
   Prepare(Stmt, DB, "INSERT INTO users (name, age) VALUES (?, ?)");
   
   -- Bind values to parameters (indexed)
   Bind_Text(Stmt, 1, "Alice");
   Bind_Int(Stmt, 2, 30);
   Step(Stmt);
   
   -- Using named parameters
   Prepare(Stmt, DB, "SELECT * FROM users WHERE name = :name AND age > :min_age");
   Bind_Text(Stmt, ":name", "Alice");
   Bind_Int(Stmt, ":min_age", 25);
   
   -- Process results
   while Step(Stmt) = ROW loop
      Put_Line("Found: " & Column_Text(Stmt, 0));
   end loop;
end Statements_Example;
```

### Working with BLOBs

```ada
with Ada_Sqlite3;
with Ada_Sqlite3.Blobs;
with Ada.Streams;

procedure Blob_Example is
   use Ada_Sqlite3;
   use Ada_Sqlite3.Blobs;
   
   DB : Database;
   Stmt : Statement;
   My_Blob : Blob;
   Data : Ada.Streams.Stream_Element_Array(1..100);
begin
   Open(DB, "test.db");
   
   -- Create a BLOB
   My_Blob := Create(Data);
   
   -- Write BLOB to database
   Prepare(Stmt, DB, "INSERT INTO files (data) VALUES (?)");
   Bind_Blob(Stmt, 1, My_Blob);
   Step(Stmt);
   
   -- Read BLOB from database
   Prepare(Stmt, DB, "SELECT data FROM files WHERE id = ?");
   Bind_Int(Stmt, 1, 1);
   Step(Stmt);
   
   declare
      Retrieved_Blob : constant Blob := Column_Blob(Stmt, 0);
   begin
      -- Use stream interface to read data
      declare
         Buffer : Ada.Streams.Stream_Element_Array(1..Size(Retrieved_Blob));
         Last : Ada.Streams.Stream_Element_Offset;
      begin
         Read_Blob(Retrieved_Blob, Buffer, Last);
      end;
   end;
end Blob_Example;
```

### Working with Generic Functions

```ada
with Ada_Sqlite3;
with Ada_Sqlite3.Generic_Functions;

procedure Custom_Function_Example is
   -- Define context type for your function
   type Sum_Context is record
      Total : Long_Float := 0.0;
      Count : Natural := 0;
   end record;
   
   -- Instantiate generic package
   package Sum_Functions is new Ada_Sqlite3.Generic_Functions
     (Context_Type => Sum_Context);
   use Sum_Functions;
   
   -- Define aggregate function
   function Step_Sum(Args : Function_Args; Context : Sum_Context) return Result_Type is
      New_Context : Sum_Context := Context;
   begin
      New_Context.Total := Context.Total + Get_Double(Args, 0);
      New_Context.Count := Context.Count + 1;
      return (Kind => Double_Result, Double_Value => New_Context.Total);
   end Step_Sum;
   
   function Final_Sum(Context : Sum_Context) return Result_Type is
   begin
      return (Kind => Double_Result, 
              Double_Value => Context.Total / Long_Float(Context.Count));
   end Final_Sum;
   
   DB : Ada_Sqlite3.Database;
   Initial_Context : Sum_Context;
begin
   Ada_Sqlite3.Open(DB, "test.db");
   
   -- Register aggregate function
   Create_Aggregate(DB, "avg_custom", 1,
                   Step_Sum'Access,
                   Final_Sum'Access,
                   Initial_Context,
                   Deterministic);
   
   -- Use the function
   declare
      Stmt : Ada_Sqlite3.Statement := 
        Ada_Sqlite3.Prepare(DB, "SELECT avg_custom(value) FROM measurements");
   begin
      if Ada_Sqlite3.Step(Stmt) = Ada_Sqlite3.ROW then
         Put_Line("Average:" & Ada_Sqlite3.Column_Double(Stmt, 0)'Image);
      end if;
   end;
end Custom_Function_Example;
```

## API Reference

### Database Operations

- `Open` - Open a database connection with specified flags (READWRITE, CREATE, etc.)
- `Execute` - Execute a simple SQL statement
- `Last_Insert_Row_ID` - Get the ID of the last inserted row
- `Changes` - Get the number of rows changed by the last statement
- `Version` - Get SQLite library version
- `Is_Open` - Check if database connection is open

### Statement Operations

- `Prepare` - Prepare a SQL statement
- `Reset` - Reset a prepared statement for reuse
- `Clear_Bindings` - Clear all bindings on a prepared statement
- `Step` - Execute a prepared statement (returns ROW or DONE)
- `Column_Count` - Get number of columns in result set
- `Column_Name` - Get name of a result column
- `Column_Type` - Get datatype of a column

### Parameter Binding

- `Bind_Null(Index | Name)` - Bind NULL value
- `Bind_Int(Index | Name)` - Bind 32-bit integer
- `Bind_Int64(Index | Name)` - Bind 64-bit integer
- `Bind_Double(Index | Name)` - Bind floating-point number
- `Bind_Text(Index | Name)` - Bind String text
- `Bind_Text_UTF16(Index | Name)` - Bind Wide_String text
- `Bind_Blob` - Bind BLOB data
- `Bind_Parameter_Index` - Get index for named parameter

### Result Access

- `Column_Int` - Get 32-bit integer value
- `Column_Int64` - Get 64-bit integer value
- `Column_Double` - Get floating-point value
- `Column_Text` - Get String text value
- `Column_Text_UTF16` - Get Wide_String text value
- `Column_Blob` - Get BLOB value
- `Column_Is_Null` - Check if column value is NULL
- `Get_Column_Type` - Get datatype of column value

### BLOB Operations

- `Create` - Create new BLOB from byte array
- `Size` - Get BLOB size in bytes
- `Data` - Get raw BLOB data
- `Read_Blob` - Stream read from BLOB
- `Write_Blob` - Stream write to BLOB

### Generic Function Support

- `Create_Function` - Create scalar function
- `Create_Aggregate` - Create aggregate function
- `Create_Window` - Create window function
- Utility Types:
  - `Function_Args` - Access to function arguments
  - `Result_Type` - Function return value container
  - `Context_Type` - User-defined function state
- Function Flags:
  - `UTF8` - Function handles UTF-8 text
  - `Deterministic` - Function results are deterministic

### Error Handling

- `SQLite_Error` - Exception type for SQLite errors
- `Result_Code` - Enumeration of SQLite result codes
- `Check_Result` - Verify operation success
- `Raise_Error` - Raise exception with details

## License

Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
