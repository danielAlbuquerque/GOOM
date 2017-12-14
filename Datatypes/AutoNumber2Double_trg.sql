-- -------------------------------------------------------------------------------------------------
-- Chuck Woodbury - Senior Systems Consultant, Technology Services.
-- Chuck.Woodbury@HexagonSI.com
-- Hexagon Safety & Infrastructure / Hexagon Geospatial
-- Huntsville, Alabama 35894
-- 256-730-7755
--
-- Copyright (c) 2016, Charles Woodbury
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without modification, are permitted 
-- provided that the copyright notice, and the following disclaimer are included.
-- --------------------------------------------------------------------------------------------------
-- DISCLAIMER
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR 
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND 
-- FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
-- CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL 
-- DAMAGES (NOT LIMITED TO THE LOSS OF USE, DATA, OR PROFITS, OR BUSINESS INTERRUPTION HOWEVER
-- CAUSED INCLUDING, AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, 
-- OR TORT, INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, 
-- EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- -------------------------------------------------------------------------------------------------------
-- Note:  GOOM Package must be installed before using this script.
-- -------------------------------------------------------------------------------------------------------
-- @AutoNumber2Double_trg
-- This trigger should be installed in GDOSYS and is AFM and SFM compatible.
-- In order for the trigger to work, GDOSYS must have the following privilege:
--   grant select any table to GDOSYS;
-- This trigger checks incoming feature classes whose primary key should be double but is set 
-- to integer due to the presence of an autonumber sequence.  The type 4 integer in 
-- GDOSYS.GFIELDMAPPING is converted to a type 7 double for the inserted feature class.
--
-- While this trigger will work for most feature classes, it may cause an issue with the 
-- connectivily tables used in AFM.  Three column connectivity tables are required to have
-- an autonumber primary key or a warning will appear in feature model.  the connectivity
-- table will still work OK, you will just have to live with the warning.  If you create
-- connectivity tables through Feature Model, then there is no issue.
-- ----------------------------------------------------------------------------------------
-- History:  02/23/2006  Initial Creation
-- -----------------------------------------------------------------------------------------
CREATE OR REPLACE TRIGGER AUTONUMBER2DOUBLE_TRG
BEFORE INSERT ON GDOSYS.GFIELDMAPPING REFERENCING NEW AS NEW FOR EACH ROW
DECLARE
  v_count INTEGER := 0;
BEGIN
  SELECT COUNT(1) INTO v_count
    FROM ALL_IND_COLUMNS
   WHERE COLUMN_NAME IN (SELECT COLUMN_NAME
                           FROM ALL_TAB_COLUMNS
                          WHERE OWNER = :NEW.OWNER
                            AND DATA_PRECISION > 9
                            AND DATA_SCALE = 0)
     AND INDEX_NAME IN (SELECT CONSTRAINT_NAME
                          FROM ALL_CONSTRAINTS
                         WHERE OWNER = :NEW.OWNER
                           AND CONSTRAINT_TYPE = 'P')
     AND INDEX_OWNER = :NEW.OWNER
     AND COLUMN_NAME = :NEW.COLUMN_NAME
     AND TABLE_NAME  = :NEW.TABLE_NAME;
  --
  IF v_count > 0 THEN
    :NEW.DATA_TYPE := 7;
  END IF;
  --
END;
/