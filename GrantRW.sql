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
-- @GrantRW
-- Grant RW privileges to specified user.
-- -----------------------------------------------------------------------------------------
-- History:  01/01/2005  Created.
--           04/19/2005  Updated by Lars Eggan.
--           07/02.2005  Updated.
--           04/17/2007  Updated.
--           06/17/2014  Updated.
------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TITLELINE('Grants Read-Write privileges on all schema objects to indicated user');
EXEC GOOM.DBLLINE;
ACCEPT user CHAR PROMPT  'Grant the following user Read-Write Privileges ->';
EXEC GOOM.DOTLINE;
set verify off
--
DECLARE
CURSOR cur_all_objects is 
        SELECT OBJECT_NAME FROM USER_OBJECTS 
         WHERE OBJECT_TYPE IN ('SEQUENCE','TABLE','VIEW') 
           AND OBJECT_NAME NOT IN ('XDB')
           AND OBJECT_NAME not like 'BIN$%';
c_proc    VARCHAR2(12):='GrantRW';
v_user    VARCHAR2(32):=UPPER('&user');
v_object  cur_all_objects%ROWTYPE;
v_sql     VARCHAR2(255);
BEGIN
  FOR v_object IN cur_all_objects LOOP
  v_sql:='GRANT ALL ON '||v_object.object_name||' TO '||v_user;
  DBMS_OUTPUT.PUT_LINE (v_sql); -- For Debug use
  EXECUTE IMMEDIATE v_sql;
  END LOOP;
    EXCEPTION
       WHEN OTHERS THEN
         GOOM.REPORT_ERROR( c_proc,v_object.object_name,v_sql,SQLCODE,SQLERRM);
END;
/
PROMPT All privileges have been granted to &&user;
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
-- ----------------------------------------------------------------------------------------