--
-- \brief  Test suite
-- \author Alexander Senier
-- \date   2019-02-16
--
-- Copyright (C) 2018 Componolit GmbH
--
-- This file is part of SXML, which is distributed under the terms of the
-- GNU Affero General Public License version 3.
--

-- Import tests and sub-suites to run
with Test_Cases;

package body Test_Suite is

   use AUnit;

   -- Statically allocate test suite:
   Result : aliased AUnit.Test_Suites.Test_Suite;

   --  Statically allocate test cases:
   Tests : aliased Test_Cases.Test_Case;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Test_Suites.Add_Test (Result'Access, Tests'Access);
      return Result'Access;
   end Suite;

end Test_Suite;

