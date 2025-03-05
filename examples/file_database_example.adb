-------------------------------------------------------------------------------
-- File database example demonstrating the use of Ada_Sqlite3 with a file-based database
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Directories;
with Ada_Sqlite3;

procedure File_Database_Example is
   use Ada.Text_IO;
   use Ada_Sqlite3;

   Result : Result_Code;
   DB_Filename : constant String := "example.db";

   --  Helper procedure to print all products
   procedure Print_Products (DB : in out Database) is
      Query_Stmt : Statement;
   begin
      Put_Line ("Products:");
      Put_Line ("ID | Name                 | Price    | Stock");
      Put_Line ("---+----------------------+----------+------");

      Query_Stmt := Prepare (DB, "SELECT id, name, price, stock FROM products ORDER BY id");

      loop
         Result := Step (Query_Stmt);
         exit when Result = DONE;

         if Result = ROW then
            declare
               ID : constant Integer := Column_Int (Query_Stmt, 0);
               Name : constant String := Column_Text (Query_Stmt, 1);
               Price : constant Float := Column_Double (Query_Stmt, 2);
               Stock : constant Integer := Column_Int (Query_Stmt, 3);
               
               --  Format the output with proper alignment
               ID_Str : constant String := Integer'Image (ID);
               Price_Str : String (1 .. 8);
            begin
               --  Format the price with 2 decimal places
               declare
                  Price_Int : constant Integer := Integer (Price * 100.0);
                  Dollars : constant Integer := Price_Int / 100;
                  Cents : constant Integer := Price_Int mod 100;
                  Cents_Str : String := Integer'Image (Cents);
               begin
                  if Cents < 10 then
                     Cents_Str := "0" & Integer'Image (Cents) (2 .. 2);
                  else
                     Cents_Str := Cents_Str (2 .. 3);
                  end if;
                  
                  Price_Str := "$" & Integer'Image (Dollars) (2 .. Integer'Image (Dollars)'Last) & 
                               "." & Cents_Str;
               end;
               
               Put (ID_Str & " | ");
               
               --  Ensure name fits in the column (20 chars)
               if Name'Length <= 20 then
                  Put (Name);
                  for I in Name'Length + 1 .. 20 loop
                     Put (" ");
                  end loop;
               else
                  Put (Name (Name'First .. Name'First + 17) & "...");
               end if;
               
               Put (" | " & Price_Str & " | ");
               Put_Line (Integer'Image (Stock));
            end;
         end if;
      end loop;

      New_Line;
   end Print_Products;

begin
   Put_Line ("SQLite3 File Database Example");
   
   declare
      DB : Database;
   begin
      --  Check if the database file already exists
      if Ada.Directories.Exists (DB_Filename) then
         Put_Line ("Opening existing database: " & DB_Filename);
         DB := Open (DB_Filename, OPEN_READWRITE);
      else
         Put_Line ("Creating new database: " & DB_Filename);
         DB := Open (DB_Filename, OPEN_READWRITE or OPEN_CREATE);
         
         --  Create the schema
         Put_Line ("Creating schema...");
         Execute (DB, "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT, price REAL, stock INTEGER)");
         
         --  Insert initial data
         Put_Line ("Inserting initial data...");
         Execute (DB, "INSERT INTO products (name, price, stock) VALUES ('Laptop', 999.99, 10)");
         Execute (DB, "INSERT INTO products (name, price, stock) VALUES ('Smartphone', 699.99, 20)");
         Execute (DB, "INSERT INTO products (name, price, stock) VALUES ('Tablet', 349.99, 15)");
         Execute (DB, "INSERT INTO products (name, price, stock) VALUES ('Headphones', 149.99, 30)");
         Execute (DB, "INSERT INTO products (name, price, stock) VALUES ('Keyboard', 79.99, 25)");
      end if;
      
      --  Print all products
      Print_Products (DB);
      
      --  Add a new product using a prepared statement
      Put_Line ("Adding a new product...");
      declare
         Stmt : Statement := Prepare (DB, "INSERT INTO products (name, price, stock) VALUES (?, ?, ?)");
      begin
         Bind_Text (Stmt, 1, "Mouse");
         Bind_Double (Stmt, 2, 49.99);
         Bind_Int (Stmt, 3, 50);
         Step (Stmt);
      end;
      
      --  Print all products after adding
      Print_Products (DB);
      
      --  Update a product
      Put_Line ("Updating product stock...");
      declare
         Stmt : Statement := Prepare (DB, "UPDATE products SET stock = stock - ? WHERE name = ?");
      begin
         Bind_Int (Stmt, 1, 5);
         Bind_Text (Stmt, 2, "Laptop");
         Step (Stmt);
      end;
      
      --  Print all products after updating
      Print_Products (DB);
      
      --  Get the total value of inventory
      Put_Line ("Calculating total inventory value...");
      declare
         Stmt : Statement := Prepare (DB, "SELECT SUM(price * stock) FROM products");
      begin
         if Step (Stmt) = ROW then
            declare
               Total_Value : constant Float := Column_Double (Stmt, 0);
            begin
               Put_Line ("Total inventory value: $" & 
                        Integer'Image (Integer (Total_Value)) (2 .. Integer'Image (Integer (Total_Value))'Last) &
                        "." & Integer'Image (Integer ((Total_Value - Float'Floor (Total_Value)) * 100.0)) (2 .. 3));
            end;
         end if;
         
      end;
      
      --  Clean up
      Put_Line ("Database file: " & DB_Filename);
   end;

exception
   when E : SQLite_Error =>
      Put_Line ("SQLite error: " & Ada.Exceptions.Exception_Message (E));
   when E : others =>
      Put_Line ("Error: " & Ada.Exceptions.Exception_Message (E));
end File_Database_Example;
