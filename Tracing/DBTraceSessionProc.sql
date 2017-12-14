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
-- This procedure is for Oracle 10g or later.
-- SYSTEM OR YOUR DBA USER MUST HAVE THE FOLLOWING GRANTED FROM SYS:
-- GRANT EXECUTE ON DBMS_MONITOR TO SYSTEM;
-- GRANT SELECT ANY DICTIONARY TO SYSTEM;
--
-- EXEC TraceSession(sid, serial, '<action>');
-- Enables session tracing for the sid and serial number.
-- Set <action> to 'ON' to activate and 'OFF' to stop.
-- -------------------------------------------------------
CREATE OR REPLACE PROCEDURE TraceSession(v_sid in NUMBER,v_serial in NUMBER,v_action IN VARCHAR2 DEFAULT 'ON') AUTHID CURRENT_USER
IS
  c_cmdname          CONSTANT VARCHAR2(12):='TraceSession';
  c_sessionerr       VARCHAR2(64) := 'No session found for sid and serial# specified';
  c_otherErr         VARCHAR2(32) := 'General Error Occurred';
  c_dest             VARCHAR2(14) := 'user_dump_dest'; 
  c_trace            VARCHAR2(12) := 'No Trace'; 
  c_tracestart       VARCHAR2(24) := 'Tracing initiated for: ';
  c_traceend         VARCHAR2(24) := 'Tracing ended for: ';
  --
  v_user             VARCHAR2(32);
  v_spid             NUMBER; 
  v_dump_dest        VARCHAR2(200);
  v_dbname           VARCHAR2(32);
  v_version          VARCHAR2(32);
  v_compatible       VARCHAR2(32);
  v_filename         VARCHAR2(32);
  v_path             VARCHAR2(255);
  v_sql              VARCHAR2(255);
  v_no_session_found EXCEPTION;
BEGIN
    v_dbname:=GOOM.GetDBNAME;
    IF ( v_sid = 0 ) THEN
      GOOM.Response( c_cmdname,'Trace Processing Cancelled.');
      RETURN;
    END IF;
    BEGIN
      v_sql:='SELECT A.USERNAME, B.SPID FROM V$SESSION A, V$PROCESS B WHERE A.SID = :vsid AND A.SERIAL# = :vserial AND A.PADDR = B.ADDR';         
      EXECUTE IMMEDIATE v_sql into v_user,v_spid USING v_sid, v_serial;
    EXCEPTION
      WHEN NO_DATA_FOUND then
      RAISE v_no_session_found;
    END;
    EXECUTE IMMEDIATE 'SELECT VALUE FROM V$PARAMETER WHERE NAME = ''user_dump_dest''' INTO v_dump_dest ;
    v_filename := v_dbname || '_ora_' || v_spid || '.trc';
    v_path:=v_dump_dest||'\'||v_filename;
    IF UPPER(v_action) = 'ON' THEN
       SYS.DBMS_MONITOR.SESSION_TRACE_ENABLE(v_sid,v_serial,TRUE,TRUE);
       c_trace:='TRACING ON';       
       GOOM.Response(c_trace,c_tracestart||v_user||': '||TO_CHAR(SYSDATE, 'MM-DD-YYYY HH24:MI:SS'));
    ELSE
       SYS.DBMS_MONITOR.SESSION_TRACE_DISABLE(v_sid,v_serial);
       c_trace:='TRACING OFF'; 
       GOOM.Response(c_trace,c_traceend||v_user||': '||TO_CHAR(SYSDATE, 'MM-DD-YYYY HH24:MI:SS'));
    END IF;
    GOOM.Response('TRACING OUTPUT','Session Trace File: '||v_path);
EXCEPTION
  WHEN v_no_session_found THEN
    GOOM.REPORT_ERROR (c_cmdname,c_sessionerr,c_trace,sqlcode,sqlerrm);
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR (c_cmdname,c_otherErr,c_trace,sqlcode,sqlerrm);
END TraceSession;
/
show errors
-- ------------------------------------------------------------------