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
-- --------------------------------------------------------------------------------------------------------
-- History: 02/23/2006  Initial Creation
-- ----------------------------------------------------------------------------------------
-- The type 4 integer in GDOSYS.GFIELDMAPPING is converted to a type 7 double.
-- This procedure should be run once inthe USER that owns the feature classes in question.
-- -----------------------------------------------------------------------------------------
DECLARE
CURSOR GetGfield IS 
        SELECT TABLE_NAME, COLUMN_NAME 
          FROM GDOSYS.GFIELDMAPPING 
         WHERE OWNER=USER
           AND DATA_TYPE=7
           AND COLUMN_NAME IN 
               (SELECT COLUMN_NAME 
                  FROM COLS 
                 WHERE DATA_PRECISION=10
                   AND DATA_SCALE=0);
--
v_gfield       GetGfield%ROWTYPE;
v_sql          VARCHAR2(255);
v_count        PLS_INTEGER:=0;
BEGIN
FOR v_gfield IN GetGfield LOOP
  v_sql:='UPDATE GDOSYS.GFIELDMAPPING SET DATA_TYPE=4 WHERE TABLE_NAME=:tname AND column_name=:col';
  EXECUTE IMMEDIATE v_sql USING v_gfield.table_name, v_gfield.column_name;
  v_count:=v_count+1;
END LOOP;
  COMMIT;
  DBMS_OUTPUT.PUT_LINE('GFIELDMAPPING entries changed:'||v_count);
EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('Process Failed: '||SQLCODE);
    ROLLBACK;
END;
/
-- -----------------------------------------------------------------------------------------