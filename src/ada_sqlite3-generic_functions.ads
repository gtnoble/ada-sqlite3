with Ada.Streams;
with Ada.Containers.Indefinite_Holders;
with Ada_Sqlite3.Low_Level;
with Interfaces.C;
use type Ada.Streams.Stream_Element_Array;  -- Make equality operator visible
use type Interfaces.C.size_t;

generic
   type Context_Type (<>) is private;
package Ada_Sqlite3.Generic_Functions is
   package Context_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Context_Type);
   type Function_Type is (Scalar_Function, Aggregate_Function, Window_Function);
   type Function_Flags is mod 2**32;
   
   UTF8          : constant Function_Flags := 1;
   Deterministic : constant Function_Flags := 16#800#;

   -- Container packages for variable-length data
   package String_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => String);
   package Wide_String_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Wide_String);
   package Blob_Holders is new Ada.Containers.Indefinite_Holders
     (Element_Type => Ada.Streams.Stream_Element_Array);

   -- Argument access
   subtype Function_Argc is Natural range Natural(Low_Level.Sqlite_Argc'First) .. Natural(Low_Level.Sqlite_Argc'Last);
   type Function_Args (Length : Function_Argc) is private;
   function Arg_Count (Args : Function_Args) return Natural;
   function Get_Type (Args : Function_Args; Index : Natural) return Ada_Sqlite3.Low_Level.Datatype;

   -- Value getters
   function Get_Int (Args : Function_Args; Index : Natural) return Integer;
   function Get_Int64 (Args : Function_Args; Index : Natural) return Long_Long_Integer;
   function Get_Double (Args : Function_Args; Index : Natural) return Long_Float;
   function Get_Text (Args : Function_Args; Index : Natural) return String;
   function Get_Text_UTF8 (Args : Function_Args; Index : Natural) return String;
   function Get_Text_UTF16 (Args : Function_Args; Index : Natural) return Wide_String;
   function Get_Blob (Args : Function_Args; Index : Natural) return Ada.Streams.Stream_Element_Array;
   function Get_Blob_Length (Args : Function_Args; Index : Natural) return Natural;
   function Is_Null (Args : Function_Args; Index : Natural) return Boolean;
   
   -- Numeric conversion functions
   function Value_Bytes (Args : Function_Args; Index : Natural) return Integer;
   function Value_As_Double (Args : Function_Args; Index : Natural) return Long_Float;
   function Value_As_Int (Args : Function_Args; Index : Natural) return Integer;
   function Value_As_Int64 (Args : Function_Args; Index : Natural) return Long_Long_Integer;

   -- Result types
   type Result_Kind is (
      Null_Result,
      Int_Result,
      Int64_Result,
      Float_Result,
      Double_Result,
      Text_Result,
      Text_UTF8_Result,
      Text_UTF16_Result,
      Text_UTF16BE_Result,
      Text_UTF16LE_Result,
      Blob_Result,
      Zeroblob_Result
   );

   type Result_Type (Kind : Result_Kind := Null_Result) is record
      case Kind is
         when Int_Result =>
            Int_Value : Integer;
         when Int64_Result =>
            Int64_Value : Long_Long_Integer;
         when Float_Result =>
            Float_Value : Float;
         when Double_Result =>
            Double_Value : Long_Float;
         when Text_Result | Text_UTF8_Result =>
            Text_Value : String_Holders.Holder;
         when Text_UTF16_Result | Text_UTF16BE_Result | Text_UTF16LE_Result =>
            Wide_Text_Value : Wide_String_Holders.Holder;
         when Blob_Result =>
            Blob_Value : Blob_Holders.Holder;
         when Zeroblob_Result =>
            Zeroblob_Size : Natural;
         when others =>
            null;
      end case;
   end record;

   -- Unified callback types
   type Function_Callback is access 
     function (Args : Function_Args; Context : Context_Type) return Result_Type;
   
   type No_Args_Callback is access 
     function (Context : Context_Type) return Result_Type;
     
   type Inverse_Procedure is access 
     procedure (Args : Function_Args; Context : Context_Type);

   -- Function creation
   procedure Create_Function
     (DB      : in out Database;
      Name    : String;
      N_Args  : Natural;
      Func    : Function_Callback;
      Context : Context_Type;
      Flags   : Function_Flags := 0);

   procedure Create_Aggregate
     (DB         : in out Database;
      Name       : String;
      N_Args     : Natural;
      Step_Func  : Function_Callback;
      Final_Func : No_Args_Callback;
      Context    : Context_Type;
      Flags      : Function_Flags := 0);
      
   procedure Create_Window
     (DB           : in out Database;
      Name         : String;
      N_Args       : Natural;
      Step_Func    : Function_Callback;
      Final_Func   : No_Args_Callback;
      Value_Func   : No_Args_Callback;
      Inverse_Func : Inverse_Procedure;
      Context      : Context_Type;
      Flags        : Function_Flags := 0);

private
   type Function_Args (Length : Function_Argc) is record
      Data : Low_Level.Sqlite3_Value_Array (0 .. Low_Level.Sqlite_Argc(Length)  - 1);
   end record;

   type Function_Kind is (Scalar, Aggregate, Window);
   
   type Callback_Data (Kind : Function_Kind := Scalar) is record
      case Kind is
         when Scalar =>
            Scalar_Func : Function_Callback;
         when Aggregate =>
            Step_Func  : Function_Callback;
            Final_Func : No_Args_Callback;
         when Window =>
            Window_Step    : Function_Callback;
            Window_Final   : No_Args_Callback;
            Window_Value   : No_Args_Callback;
            Window_Inverse : Inverse_Procedure;
      end case;
   end record;

   type Function_State_Record is limited record
      Context  : Context_Holders.Holder;
      Callback : Callback_Data;
   end record;
   
   type Function_State_Access is access all Function_State_Record;
   
   -- Free a Function_State_Access allocated by the package
   procedure Free_Function_State (State : in out Function_State_Access);

   -- C-compatible cleanup callback type
   type Cleanup_Callback is access procedure (Data : System.Address)
     with Convention => C;

   -- Internal wrappers with proper C calling convention
   procedure Internal_Cleanup (Data : System.Address)
     with Convention => C;
     
end Ada_Sqlite3.Generic_Functions;
