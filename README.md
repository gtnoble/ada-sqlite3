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
      
      Finalize(Stmt);
   end;

   -- Close the database
   Close(DB);
end Example;
```

### Working with Prepared Statements

```ada
with Ada_Sqlite3;

procedure Insert_Example is
   use Ada_Sqlite3;

   DB : Database;
   Stmt : Statement;
begin
   Open(DB, "test.db");
   
   -- Prepare a statement with parameters
   Prepare(Stmt, DB, "INSERT INTO users (name, age) VALUES (?, ?)");
   
   -- Bind values to parameters
   Bind_Text(Stmt, 1, "Alice");
   Bind_Int(Stmt, 2, 30);
   
   -- Execute the statement
   Step(Stmt);
   
   -- Reset and reuse the statement with new values
   Reset(Stmt);
   Clear_Bindings(Stmt);
   
   Bind_Text(Stmt, 1, "Bob");
   Bind_Int(Stmt, 2, 25);
   Step(Stmt);
   
   Finalize(Stmt);
   Close(DB);
end Insert_Example;
```

## API Reference

### Database Operations

- `Open` - Open a database connection
- `Execute` - Execute a simple SQL statement
- `Last_Insert_Row_ID` - Get the ID of the last inserted row
- `Changes` - Get the number of rows changed by the last statement

### Statement Operations

- `Prepare` - Prepare a SQL statement
- `Reset` - Reset a prepared statement
- `Clear_Bindings` - Clear all bindings on a prepared statement
- `Step` - Execute a prepared statement
- `Bind_*` - Bind values to parameters
- `Column_*` - Get values from result columns

## License

Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.
