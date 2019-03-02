--
-- @summary Test utils
-- @author  Alexander Senier
-- @date    2019-02-27
--
-- Copyright (C) 2019 Componolit GmbH
--

with Ada.Directories;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO.Text_Streams;

package body Util
is
   type String_Access is access all String;
   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return access String
   is
      Block_Size : constant := 1024;
      use Ada.Text_IO;
      use Ada.Text_IO.Text_Streams;
      File_Size : Natural := Natural (Ada.Directories.Size (Name));
      Result : access String := new String (1 .. File_Size);
      File : File_Type;
      Len  : Natural;
      Off  : Natural := 1;
   begin
      Open (File, In_File, Name);
      loop
         Len := (if File_Size - Off > Block_Size then Block_Size else File_Size - Off + 1);
         String'Read (Stream (File), Result.all (Off .. Off + Len - 1));
         Off := Off + Len;
         exit when Off >= File_Size;
      end loop;
      Close (File);
      return Result;
   end Read_File;

end Util;
