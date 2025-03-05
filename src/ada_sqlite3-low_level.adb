-------------------------------------------------------------------------------
-- Ada_Sqlite3.Low_Level - Low-level SQLite3 C API bindings
--
-- Copyright (c) 2025 Garret Noble
--
-- Licensed under the terms of the MIT License or Apache-2.0 with LLVM exception
-------------------------------------------------------------------------------

package body Ada_Sqlite3.Low_Level is

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

end Ada_Sqlite3.Low_Level;
