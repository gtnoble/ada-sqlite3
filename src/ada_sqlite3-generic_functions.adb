with System;
with System.Address_To_Access_Conversions;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Ada_Sqlite3.Generic_Functions is
   package C renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   package LL renames Ada_Sqlite3.Low_Level;

   subtype Sqlite_Arg_Count is C.int range 0 .. 127;
   
   use type LL.Datatype;
   use type System.Address;
   use type C.size_t;
   use type C.int;
   
   -- Callback type conversions
   function To_Sqlite3_Func_Callback is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Low_Level.Sqlite3_Func_Callback);

   function To_Sqlite3_Step_Callback is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Low_Level.Sqlite3_Step_Callback);

   function To_Sqlite3_Final_Callback is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Low_Level.Sqlite3_Final_Callback);
   
   function To_Sqlite3_Destroy_Callback is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Low_Level.Sqlite3_Destroy_Callback);
   
   package Function_State_Conversions is
     new System.Address_To_Access_Conversions(Function_State_Record);
   
   procedure Free is new Ada.Unchecked_Deallocation
     (Function_State_Record, Function_State_Access);
   
   procedure Free_Function_State (State : in out Function_State_Access) is
   begin
      if State /= null then
         Free(State);
      end if;
   end Free_Function_State;
   
   -- Argument access implementation
   function Arg_Count (Args : Function_Args) return Natural is
   begin
      return Natural(Args'Length);
   end Arg_Count;

   function Get_Type (Args : Function_Args; Index : Natural) return Low_Level.Datatype is
   begin
      return Low_Level.Sqlite3_Value_Type(Args(C.size_t(Index)));
   end Get_Type;

   -- Value getters implementation
   function Get_Int (Args : Function_Args; Index : Natural) return Integer is
   begin
      return Integer(Low_Level.Sqlite3_Value_Int(Args(C.size_t(Index))));
   end Get_Int;
   
   function Get_Int64 (Args : Function_Args; Index : Natural) return Long_Long_Integer is
   begin
      return Long_Long_Integer(Low_Level.Sqlite3_Value_Int64(Args(C.size_t(Index))));
   end Get_Int64;
   
   function Get_Double (Args : Function_Args; Index : Natural) return Long_Float is
   begin
      return Long_Float(Low_Level.Sqlite3_Value_Double(Args(C.size_t(Index))));
   end Get_Double;
   
   function Get_Text (Args : Function_Args; Index : Natural) return String is
      C_Str : constant CS.chars_ptr := Low_Level.Sqlite3_Value_Text(Args(C.size_t(Index)));
   begin
      return CS.Value(C_Str);
   end Get_Text;
   
   function Get_Text_UTF8 (Args : Function_Args; Index : Natural) return String is
      C_Str : constant CS.chars_ptr := Low_Level.Sqlite3_Value_Text(Args(C.size_t(Index)));
   begin
      return CS.Value(C_Str);
   end Get_Text_UTF8;
   
   function Get_Text_UTF16 (Args : Function_Args; Index : Natural) return Wide_String is
      C_Str : constant System.Address := Low_Level.Sqlite3_Value_Text16(Args(C.size_t(Index)));
      Len   : constant Natural := Natural(Low_Level.Sqlite3_Value_Bytes16(Args(C.size_t(Index)))) / 2;
      Result : Wide_String(1..Len);
      for Result'Address use C_Str;
   begin
      return Result;
   end Get_Text_UTF16;
   
   function Get_Blob (Args : Function_Args; Index : Natural)
      return Ada.Streams.Stream_Element_Array 
   is
      Bytes : constant System.Address := Low_Level.Sqlite3_Value_Blob(Args(C.size_t(Index)));
      Size  : constant Natural := Get_Blob_Length(Args, Index);
      Result : Ada.Streams.Stream_Element_Array(1..Ada.Streams.Stream_Element_Offset(Size));
      for Result'Address use Bytes;
   begin
      return Result;
   end Get_Blob;
   
   function Get_Blob_Length (Args : Function_Args; Index : Natural) return Natural is
   begin
      return Natural(Low_Level.Sqlite3_Value_Bytes(Args(C.size_t(Index))));
   end Get_Blob_Length;
   
   function Is_Null (Args : Function_Args; Index : Natural) return Boolean is
   begin
      return Get_Type(Args, Index) = Low_Level.SQLITE_NULL;
   end Is_Null;
   
   -- Numeric conversion functions
   function Value_Bytes (Args : Function_Args; Index : Natural) return Integer is
   begin
      return Integer(Low_Level.Sqlite3_Value_Bytes(Args(C.size_t(Index))));
   end Value_Bytes;
   
   function Value_As_Double (Args : Function_Args; Index : Natural) return Long_Float is
   begin
      return Long_Float(Low_Level.Sqlite3_Value_Double(Args(C.size_t(Index))));
   end Value_As_Double;
   
   function Value_As_Int (Args : Function_Args; Index : Natural) return Integer is
   begin
      return Integer(Low_Level.Sqlite3_Value_Int(Args(C.size_t(Index))));
   end Value_As_Int;
   
   function Value_As_Int64 (Args : Function_Args; Index : Natural) return Long_Long_Integer is
   begin
      return Long_Long_Integer(Low_Level.Sqlite3_Value_Int64(Args(C.size_t(Index))));
   end Value_As_Int64;
   
   -- Helper procedures to set results based on Result_Type
   procedure Set_Result (Context_Handle : Low_Level.Sqlite3_Context; Result : Result_Type) is
   begin
      case Result.Kind is
         when Null_Result =>
            Low_Level.Sqlite3_Result_Null(Context_Handle);
         
         when Int_Result =>
            Low_Level.Sqlite3_Result_Int(Context_Handle, C.int(Result.Int_Value));
         
         when Int64_Result =>
            Low_Level.Sqlite3_Result_Int64(Context_Handle, C.long_long(Result.Int64_Value));
         
         when Float_Result =>
            Low_Level.Sqlite3_Result_Double(Context_Handle, C.double(Long_Float(Result.Float_Value)));
         
         when Double_Result =>
            Low_Level.Sqlite3_Result_Double(Context_Handle, C.double(Result.Double_Value));
         
         when Text_Result | Text_UTF8_Result =>
            declare
               Text_Copy : aliased constant String := String_Holders.Element(Result.Text_Value);
               C_Value : constant CS.chars_ptr := CS.New_String(Text_Copy);
            begin
               Low_Level.Sqlite3_Result_Text(
                  Context_Handle, C_Value, C.int(Text_Copy'Length),
                  Low_Level.Free_Chars_Ptr_Address);
            end;
         
         when Text_UTF16_Result | Text_UTF16BE_Result | Text_UTF16LE_Result =>
            declare
               Text_Copy : aliased Wide_String := Wide_String_Holders.Element(Result.Wide_Text_Value);
            begin
               Low_Level.Sqlite3_Result_Text16(
                  Context_Handle,
                  Text_Copy'Address,
                  C.int(Text_Copy'Length * 2),
                  Low_Level.SQLITE_TRANSIENT);
            end;
         
         when Blob_Result =>
            declare
               Data_Copy : aliased Ada.Streams.Stream_Element_Array := 
                  Blob_Holders.Element(Result.Blob_Value);
            begin
               Low_Level.Sqlite3_Result_Blob(
                  Context_Handle,
                  Data_Copy'Address,
                  C.int(Data_Copy'Length),
                  Low_Level.SQLITE_TRANSIENT);
            end;
         
         when Zeroblob_Result =>
            Low_Level.Sqlite3_Result_Zeroblob(Context_Handle, C.int(Result.Zeroblob_Size));
      end case;
   end Set_Result;
   
   -- Internal cleanup implementation
   procedure Internal_Cleanup (Data : System.Address) is
      State : Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(Data));
   begin
      Free_Function_State(State);
   end Internal_Cleanup;

   -- Type conversion functions for SQLite3 callbacks
   function To_Sqlite3_Window_Callback is new Ada.Unchecked_Conversion(
      Source => System.Address,
      Target => Low_Level.Sqlite3_Window_Callback);

   -- Function wrapper implementations
   procedure Scalar_Callback_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array);
   
   procedure Scalar_Callback_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
      pragma Assert (Argv /= null or else Argc = 0, "SQLite passed null argv with non-zero count");
      
      Args : aliased constant Function_Args := 
        (if Argc = 0 then Function_Args'(1..0 => Low_Level.Null_Sqlite3_Value)
         else Argv.all(0 .. C.size_t(Argc)-1));
      User_Data : constant System.Address := Low_Level.Sqlite3_User_Data(Context);
      State : constant Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(User_Data));
      Result : Result_Type;
   begin
      Result := State.Callback.Scalar_Func(Args, Context_Holders.Element(State.Context));
      Set_Result(Context, Result);
   end Scalar_Callback_Implementation;
   
   procedure Scalar_Callback_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array) with
      Convention => C;
   
   procedure Scalar_Callback_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
   begin
      -- Convert C.int to Sqlite_Arg_Count, which will raise Constraint_Error if out of range
      Scalar_Callback_Implementation(Context, Sqlite_Arg_Count(Argc), Argv);
   end Scalar_Callback_Wrapper;

   procedure Aggregate_Step_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array);

   procedure Aggregate_Step_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
      pragma Assert (Argv /= null or else Argc = 0, "SQLite passed null argv with non-zero count");
      
      Args : aliased constant Function_Args := 
        (if Argc = 0 then Function_Args'(1..0 => Low_Level.Null_Sqlite3_Value)
         else Argv.all(0 .. C.size_t(Argc)-1));
      User_Data : constant System.Address := Low_Level.Sqlite3_User_Data(Context);
      State : constant Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(User_Data));
      Result : Result_Type;
   begin
      Result := State.Callback.Step_Func(Args, Context_Holders.Element(State.Context));
      Set_Result(Context, Result);
   end Aggregate_Step_Implementation;
   
   procedure Aggregate_Step_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array) with
      Convention => C;
   
   procedure Aggregate_Step_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
   begin
      -- Convert C.int to Sqlite_Arg_Count, which will raise Constraint_Error if out of range
      Aggregate_Step_Implementation(Context, Sqlite_Arg_Count(Argc), Argv);
   end Aggregate_Step_Wrapper;
   
   procedure Aggregate_Final_Wrapper
     (Context : Low_Level.Sqlite3_Context) with
      Convention => C;
   
   procedure Aggregate_Final_Wrapper
     (Context : Low_Level.Sqlite3_Context)
   is
      User_Data : constant System.Address := Low_Level.Sqlite3_User_Data(Context);
      State : constant Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(User_Data));
      Result : Result_Type;
   begin
      Result := State.Callback.Final_Func(Context_Holders.Element(State.Context));
      Set_Result(Context, Result);
   end Aggregate_Final_Wrapper;
   
   procedure Window_Value_Wrapper
     (Context : Low_Level.Sqlite3_Context) with
      Convention => C;
      
   procedure Window_Inverse_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array);

   procedure Window_Inverse_Implementation
     (Context : Low_Level.Sqlite3_Context;
      Argc    : Sqlite_Arg_Count;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
      pragma Assert (Argv /= null or else Argc = 0, "SQLite passed null argv with non-zero count");
      
      Args : aliased constant Function_Args := 
        (if Argc = 0 then Function_Args'(1..0 => Low_Level.Null_Sqlite3_Value)
         else Argv.all(0 .. C.size_t(Argc)-1));
      User_Data : constant System.Address := Low_Level.Sqlite3_User_Data(Context);
      State : constant Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(User_Data));
   begin
      State.Callback.Window_Inverse(Args, Context_Holders.Element(State.Context));
   end Window_Inverse_Implementation;

   procedure Window_Inverse_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array) with
      Convention => C;
   
   procedure Window_Inverse_Wrapper
     (Context : Low_Level.Sqlite3_Context;
      Argc    : C.int;
      Argv    : access Low_Level.Sqlite3_Value_Array)
   is
   begin
      -- Convert C.int to Sqlite_Arg_Count, which will raise Constraint_Error if out of range
      Window_Inverse_Implementation(Context, Sqlite_Arg_Count(Argc), Argv);
   end Window_Inverse_Wrapper;
      
   procedure Window_Value_Wrapper
     (Context : Low_Level.Sqlite3_Context)
   is
      User_Data : constant System.Address := Low_Level.Sqlite3_User_Data(Context);
      State : constant Function_State_Access := Function_State_Access(Function_State_Conversions.To_Pointer(User_Data));
      Result : Result_Type;
   begin
      Result := State.Callback.Window_Value(Context_Holders.Element(State.Context));
      Set_Result(Context, Result);
   end Window_Value_Wrapper;

   -- Function creation implementation   
   procedure Create_Function
     (DB      : in out Database;
      Name    : String;
      N_Args  : Natural;
      Func    : Function_Callback;
      Context : Context_Type;
      Flags   : Function_Flags := 0)
   is
      C_Name : CS.chars_ptr := CS.New_String(Name);
      State : constant Function_State_Access := new Function_State_Record'
        (Context  => Context_Holders.To_Holder(Context),
         Callback => (Kind => Scalar, Scalar_Func => Func));
   begin
      declare
         Scalar_CB : constant System.Address := Scalar_Callback_Wrapper'Address;
         Cleanup_CB : constant System.Address := Internal_Cleanup'Address;
         Result : constant Low_Level.Result_Code := Low_Level.Sqlite3_Create_Function_V2(
            LL.To_Sqlite3(DB.Handle),
            C_Name,
            C.int(N_Args),
            C.int(Flags),
            Function_State_Conversions.To_Address(Function_State_Conversions.Object_Pointer(State)),
            To_Sqlite3_Func_Callback(Scalar_CB),
            null,
            null,
            To_Sqlite3_Destroy_Callback(Cleanup_CB));
      begin
         if Result /= Low_Level.SQLITE_OK then
            raise SQLite_Error with "Failed to create scalar function";
         end if;
      end;
      CS.Free(C_Name);
   end Create_Function;
   
   procedure Create_Aggregate
     (DB         : in out Database;
      Name       : String;
      N_Args     : Natural;
      Step_Func  : Function_Callback;
      Final_Func : No_Args_Callback;
      Context    : Context_Type;
      Flags      : Function_Flags := 0)
   is
      C_Name : CS.chars_ptr := CS.New_String(Name);
      State : constant Function_State_Access := new Function_State_Record'
        (Context  => Context_Holders.To_Holder(Context),
         Callback => (Kind       => Aggregate,
                     Step_Func  => Step_Func,
                     Final_Func => Final_Func));
   begin
      declare
         Step_CB : constant System.Address := Aggregate_Step_Wrapper'Address;
         Final_CB : constant System.Address := Aggregate_Final_Wrapper'Address;
         Cleanup_CB : constant System.Address := Internal_Cleanup'Address;
         Result : constant Low_Level.Result_Code := Low_Level.Sqlite3_Create_Function_V2(
            LL.To_Sqlite3(DB.Handle),
            C_Name,
            C.int(N_Args),
            C.int(Flags),
            Function_State_Conversions.To_Address(Function_State_Conversions.Object_Pointer(State)),
            null,
            To_Sqlite3_Step_Callback(Step_CB),
            To_Sqlite3_Final_Callback(Final_CB),
            To_Sqlite3_Destroy_Callback(Cleanup_CB));
      begin
         if Result /= Low_Level.SQLITE_OK then
            raise SQLite_Error with "Failed to create aggregate function";
         end if;
      end;
      CS.Free(C_Name);
   end Create_Aggregate;
   
   procedure Create_Window
     (DB           : in out Database;
      Name         : String;
      N_Args       : Natural;
      Step_Func    : Function_Callback;
      Final_Func   : No_Args_Callback;
      Value_Func   : No_Args_Callback;
      Inverse_Func : Inverse_Procedure;
      Context      : Context_Type;
      Flags        : Function_Flags := 0)
   is
      C_Name : CS.chars_ptr := CS.New_String(Name);
      State : constant Function_State_Access := new Function_State_Record'
        (Context  => Context_Holders.To_Holder(Context),
         Callback => (Kind           => Window,
                     Window_Step    => Step_Func,
                     Window_Final   => Final_Func,
                     Window_Value   => Value_Func,
                     Window_Inverse => Inverse_Func));
   begin
      declare
         Step_CB : constant System.Address := Aggregate_Step_Wrapper'Address;
         Final_CB : constant System.Address := Aggregate_Final_Wrapper'Address;
         Value_CB : constant System.Address := Window_Value_Wrapper'Address;
         Inverse_CB : constant System.Address := Window_Inverse_Wrapper'Address;
         Cleanup_CB : constant System.Address := Internal_Cleanup'Address;
         Result : constant Low_Level.Result_Code := Low_Level.Sqlite3_Create_Window_Function(
            LL.To_Sqlite3(DB.Handle),
            C_Name,
            C.int(N_Args),
            C.int(Flags),
            Function_State_Conversions.To_Address(Function_State_Conversions.Object_Pointer(State)),
            To_Sqlite3_Step_Callback(Step_CB),
            To_Sqlite3_Final_Callback(Final_CB),
            To_Sqlite3_Window_Callback(Value_CB),
            To_Sqlite3_Step_Callback(Inverse_CB),
            To_Sqlite3_Destroy_Callback(Cleanup_CB));
      begin
         if Result /= Low_Level.SQLITE_OK then
            raise SQLite_Error with "Failed to create window function";
         end if;
      end;
      CS.Free(C_Name);
   end Create_Window;

end Ada_Sqlite3.Generic_Functions;
