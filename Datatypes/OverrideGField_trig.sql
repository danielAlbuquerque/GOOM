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
-- -------------------------------------------------------------------------------------------------
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
-- @OverrideGField_TRIG
-- This trigger should be installed in GDOSYS and is NOT AFM compatible.
-- This trigger checks incoming feature classes whose primary key should be double but is set 
-- to integer due to the presence of an autonumber sequence.  The type 4 integer in 
-- GDOSYS.GFIELDMAPPING is converted to a type 7 double for the inserted feature class.
-- ----------------------------------------------------------------------------------------
-- History: 02/23/2006  Initial Creation
-- ----------------------------------------------------------------------------------------
CREATE OR REPLACE TRIGGER GDOSYS.GFIELDOVERRIDE_TRIG
AFTER INSERT ON GDOSYS.GFIELDMAPPING REFERENCING NEW AS NEW FOR EACH ROW
DECLARE
  v_count INTEGER := 0;
  v_sql   VARCHAR2(255);
BEGIN
  SELECT COUNT(1)
    INTO v_count
    FROM ALL_IND_COLUMNS
   WHERE INDEX_OWNER = :NEW.owner
     AND COLUMN_NAME = :NEW.column_name
     AND TABLE_NAME  = :NEW.table_name
     AND INDEX_NAME IN (SELECT CONSTRAINT_NAME
                          FROM ALL_CONSTRAINTS
                         WHERE OWNER = :NEW.owner
                           AND CONSTRAINT_TYPE = 'P')
     AND COLUMN_NAME IN (SELECT COLUMN_NAME
                           FROM ALL_TAB_COLUMNS
                          WHERE OWNER = :NEW.owner
                            AND DATA_PRECISION > 9
                            AND DATA_SCALE = 0);
  --
  IF v_count > 0 THEN
    v_sql := 'UPDATE GDOSYS.GFIELDMAPPING SET DATA_TYPE=7 WHERE OWNER=:owner AND TABLE_NAME=:tname AND column_name=:col';
    EXECUTE IMMEDIATE v_sql
      USING :NEW.owner, :NEW.table_name, :NEW.column_name;
    COMMIT;
  END IF;
  --
END;
/