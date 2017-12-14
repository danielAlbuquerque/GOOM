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
-- This trigger will maintain the USER_SDO_GEOM_METADATA table
-- If a table is dropped, the trigger checks for an entry in
-- USER_SDO_GEOM_METADATA. If one exists, it is deleted.
-- ----------------------------------------------------------------------------------------
CREATE or REPLACE TRIGGER UPDATE_USGM_TRIG AFTER DROP ON SCHEMA
DECLARE
v_chk    INTEGER;
BEGIN
  EXECUTE IMMEDIATE 'SELECT COUNT(1) FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :tablename' 
               INTO v_chk USING sys.dictionary_obj_name;
  IF v_chk > 0 THEN
  EXECUTE IMMEDIATE 'DELETE FROM USER_SDO_GEOM_METADATA WHERE TABLE_NAME = :tablename'
              USING sys.dictionary_obj_name;
  END IF;
END;
/