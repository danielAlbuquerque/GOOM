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
-- @GrantEDIT
-- Grant Edit Only privileges to specified user
-- ----------------------------------------------------------------------------------------
-- History: 07/02/2005  Updated.
--          04/17/2007  Updated.
--          06/17/2014  Updated.
--
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TitleLine('Grants Edit-Only privileges on all schema objects to indicated user');
EXEC GOOM.DBLLINE;
ACCEPT user CHAR PROMPT  'Grant the following user Edit-Only Privileges ->';
EXEC GOOM.DOTLINE;
--
DECLARE

CURSOR cur_all_objects is 
        SELECT OBJECT_NAME FROM USER_OBJECTS 
         WHERE OBJECT_TYPE IN ('TABLE','VIEW') 
           AND OBJECT_NAME NOT IN ('XDB')
           AND OBJECT_NAME not like 'BIN$%';

CURSOR cur_seq_objects is 
        SELECT OBJECT_NAME FROM USER_OBJECTS 
         WHERE OBJECT_TYPE IN ('SEQUENCE') 
           AND OBJECT_NAME NOT IN ('XDB')
           AND OBJECT_NAME not like 'BIN$%';


  c_proc  VARCHAR2(12):='GrantEDIT';
  v_user  VARCHAR2(32):=UPPER('&user');
  v_object cur_all_objects%ROWTYPE;
  v_sql VARCHAR2(255);
  
  v_seq       cur_seq_objects%ROWTYPE;
  v_tabperm   VARCHAR2(255);
  v_seqperm   VARCHAR2(255);

BEGIN
  FOR v_object IN cur_all_objects LOOP
    v_sql:='GRANT SELECT ON '||v_object.object_name||' TO '||v_user;
    EXECUTE IMMEDIATE v_sql;
    v_sql:='GRANT INSERT ON '||v_object.object_name||' TO '||v_user;
    EXECUTE IMMEDIATE v_sql;
    v_sql:='GRANT UPDATE ON '||v_object.object_name||' TO '||v_user;
    EXECUTE IMMEDIATE v_sql;
    v_sql:='GRANT DELETE ON '||v_object.object_name||' TO '||v_user;
    EXECUTE IMMEDIATE v_sql;
  END LOOP;
  FOR v_seq IN cur_seq_objects LOOP
    v_sql:='GRANT SELECT, ALTER ON '||v_seq.object_name||' TO '||v_user;
    EXECUTE IMMEDIATE v_sql;
  END LOOP;
  GOOM.DBMSG('Editing privileges have been granted to '||v_user||'.');
EXCEPTION
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR( c_proc,v_object.object_name,v_sql,SQLCODE,SQLERRM);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON