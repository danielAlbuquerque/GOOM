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
-- @GDOGrant
-- Grants privileges to GDOSYS based on 3 types of users; master, editor, or reader.
-- Run while connected to the GDOSYS schema.
--
-- Note:  GOOM Package must be installed before using this script.
-- ----------------------------------------------------------------------------------------
--
-- History: 07/02/2005  Modified.
--          04/14/2007  Modified.
--          12/12/2008  Can be run by DBA and applies only to GDOSYS.
--          06/17/2014  Updated.
--
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TitleLine('Grants privileges to GDOSYS based on 3 types of users; master, editor, or reader');
EXEC GOOM.DBLLINE;
ACCEPT access CHAR PROMPT 'What type of Access: (F)ull, (E)dit, or (R)ead Only. (Def: F)->' DEFAULT 'F';
ACCEPT user   CHAR PROMPT 'Grant the following user access to GDOSYS (Def: PUBLIC)      ->' DEFAULT 'PUBLIC';
EXEC GOOM.DOTLINE;
set verify off
DECLARE
Cursor GetGDOTables is SELECT object_name ,object_type
                         FROM all_objects 
                        WHERE object_type IN ('TABLE','VIEW')
                          AND OBJECT_NAME NOT IN ('XDB')
                          AND OBJECT_NAME NOT LIKE 'BIN$%'
                          AND OWNER = 'GDOSYS';
Cursor GetGDOseq is SELECT object_name 
                         FROM all_objects 
                        WHERE object_type ='SEQUENCE'
                          AND OWNER = 'GDOSYS';
  c_proc      VARCHAR2(32):='GDOGrant';
  c_owner     VARCHAR2(6):='GDOSYS';
  v_access    VARCHAR2(2):=UPPER('&access');
  v_tables    GetGDOTables%ROWTYPE;
  v_seq       GetGDOSeq%ROWTYPE;
  v_tabperm   VARCHAR2(255);
  v_modperm   VARCHAR2(255);
  v_seqperm   VARCHAR2(255);
  v_lttperm   VARCHAR2(255);
  v_sql       VARCHAR2(512);
  v_grantee   VARCHAR2(32):=UPPER('&user');
  v_lttexist  INTEGER;
  v_msg       VARCHAR2(255);
BEGIN
   CASE v_access
     WHEN 'F' THEN
       v_tabperm:='SELECT, INSERT, UPDATE, DELETE';
       v_modperm:='SELECT, INSERT, UPDATE, DELETE';
       v_lttperm:='SELECT, INSERT, UPDATE, DELETE';
       v_seqperm:='SELECT, ALTER';
       v_msg:='Full Read-Write Access on GDOSYS Granted to '||v_grantee;
     WHEN 'R' THEN
       v_tabperm:='SELECT';
       v_modperm:='SELECT';
       v_lttperm:='SELECT';
       v_seqperm:='SELECT';
       v_msg:='Read-Only Access on GDOSYS Granted to '||v_grantee;
     WHEN 'E' THEN
       v_tabperm:='SELECT';
       v_modperm:='SELECT, INSERT, UPDATE';
       v_lttperm:='SELECT, INSERT, UPDATE, DELETE';
       v_seqperm:='SELECT, ALTER';
       v_msg:='Data Editing Access on GDOSYS Granted to '||v_grantee;
     ELSE 
       GOTO exit_early;
   END CASE;
   FOR v_tables in GetGDOTables LOOP
     v_sql:='GRANT '||v_tabperm||' on GDOSYS.'||v_tables.object_name||' TO '||v_grantee;
     EXECUTE IMMEDIATE v_sql;
   END LOOP;
   FOR v_seq in GetGDOseq LOOP
     v_sql:='GRANT '||v_seqperm||' on GDOSYS.'||v_seq.object_name||' TO '||v_grantee;
     EXECUTE IMMEDIATE v_sql;
   END LOOP;
   v_sql:='GRANT '||v_modperm||' on GDOSYS.MODIFIEDTABLES TO '||v_grantee;
   EXECUTE IMMEDIATE v_sql;
   v_sql:='GRANT '||v_modperm||' on GDOSYS.MODIFICATIONLOG TO '||v_grantee;
   EXECUTE IMMEDIATE v_sql;
   SELECT count(1) INTO v_lttexist 
     FROM user_objects 
    WHERE object_name like 'LTT%' 
      AND object_type in ('TABLE','VIEW');
   IF v_lttexist>0 THEN
     v_sql:='GRANT '||v_lttperm||' on GDOSYS.LTT_REVISION_SETS_BASE TO '||v_grantee;
     EXECUTE IMMEDIATE v_sql;
     v_sql:='GRANT '||v_lttperm||' on GDOSYS.LTT_REVISION_SETS TO '||v_grantee;
     EXECUTE IMMEDIATE v_sql;
     v_sql:='GRANT '||v_lttperm||' on GDOSYS.LTT_TABLES TO '||v_grantee;
     EXECUTE IMMEDIATE v_sql;
   END IF;
   GOTO finishup;
   <<exit_early>>
   v_msg:='Process exited, check valid parameters.';
   <<finishup>>
   GOOM.DBMSG(v_msg);
    EXCEPTION
       WHEN OTHERS THEN
         GOOM.REPORT_ERROR( c_proc,v_sql,v_msg,SQLCODE,SQLERRM);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON
-- ----------------------------------------------------------------------------------------
