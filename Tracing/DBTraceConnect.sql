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
PROMPT =============================  Trace on Connect  =============================;
PROMPT This procedure will allow you to trace a specified schema when a user connects.;
PROMPT A logon trigger activates the trace on connection and a logoff trigger ends the;
PROMPT trace when the user disconnects.  You must run this as a DBA user and the DBA ;
PROMPT user must have execute privileges on both DMBS_MONITOR and DBMS_SESSION.;
PROMPT ..;
ACCEPT schema char PROMPT 'Enter the Schema to Trace --> ';
PROMPT ..............................................................................;
DECLARE
 v_schema     VARCHAR2(32):=UPPER('&schema');
--CREATE OR REPLACE 
--PROCEDURE DBTraceOnSchema(v_schema IN VARCHAR2) AUTHID DEFINER IS
 c_cmdname    VARCHAR2(15):='DBTraceConnect';
 v_trigger    VARCHAR2(512);
 v_goomchk    PLS_INTEGER;
begin
  v_trigger:='
  CREATE OR REPLACE TRIGGER TRACE_LOGON_trig AFTER LOGON ON DATABASE
  DECLARE
  uid       VARCHAR2(64);
  v_id      VARCHAR2(64);
  v_trace   VARCHAR2(64):=UPPER('''||v_schema||''');
  BEGIN
    SELECT ora_login_user ||'':''|| SYS_CONTEXT(''USERENV'', ''OS_USER'') INTO uid FROM dual;
    dbms_session.set_identifier(uid);
    v_id:=substr(uid,1,instr(uid,'':'')-1);
    IF v_id = v_trace THEN
      dbms_monitor.client_id_trace_enable(uid);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN NULL;
  END;';
  EXECUTE IMMEDIATE v_trigger;
  --
  v_trigger:='CREATE OR REPLACE TRIGGER TRACE_LOGOFF_trig before LOGOFF ON DATABASE
  DECLARE
  uid       VARCHAR2(64);
  v_id      VARCHAR2(64);
  v_trace   VARCHAR2(64):=UPPER('''||v_schema||''');
  BEGIN
    SELECT ora_login_user ||'':''|| SYS_CONTEXT(''USERENV'', ''OS_USER'') INTO uid FROM dual;
    dbms_session.set_identifier(uid);
    v_id:=substr(uid,1,instr(uid,'':'')-1);
    IF v_id = v_trace THEN
      dbms_monitor.client_id_trace_disable(uid);
    END IF;
  EXCEPTION
    WHEN OTHERS THEN NULL;
  END;';
  EXECUTE IMMEDIATE v_trigger;
  SELECT COUNT(*) INTO v_goomchk from ALL_OBJECTS WHERE OBJECT_NAME LIKE 'GOOM';
  IF v_goomchk>0 THEN
    GOOM.Response('TRIGGERS CREATED','Schema &schema is ready to trace.');
    GOOM.DBMSG('..............................................................................');
    GOOM.DBMSG('Remember to run the following when you are dome with this schema!!');
    GOOM.DBMSG('DROP TRIGGER TRACE_LOGON_trig;');
    GOOM.DBMSG('DROP TRIGGER TRACE_LOGOFF_trig;');
    GOOM.DBMSG('==============================================================================');
  ELSE
    DBMS_OUTPUT.PUT_LINE('Remember to run the following when you are done with this schema!!');
    DBMS_OUTPUT.PUT_LINE('DROP TRIGGER TRACE_LOGON_trig;');
    DBMS_OUTPUT.PUT_LINE('DROP TRIGGER TRACE_LOGOFF_trig;');
    DBMS_OUTPUT.PUT_LINE('==============================================================================');
  END IF;
END;
/


--