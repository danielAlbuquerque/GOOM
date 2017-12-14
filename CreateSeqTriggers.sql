-- -------------------------------------------------------------------------------------------------
-- Chuck Woodbury - Senior Systems Consultant, Technology Services.
-- Chuck.Woodbury@HexagonSI.com
-- Hexagon Safety & Infrastructure / Hexagon Geospatial
-- Huntsville, Alabama 35894
-- 256-730-7755
--
-- Copyright (c) 2016, Charles Woodbury
-- All rights reserved.
-- Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
-- copyright notice, and the following disclaimer are included.
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
-- History:  02/01/2006   Written for Amtrak.
--           04/17/2007   Modified.
--           06/18/2014   Modified.
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
PROMPT Creates a sequence trigger for the indicated table.  THis allows the sequence to be
PROMPT used for edits outside of GeoMedia without interfering with GM edits. 
PROMPT Enter ALL to create triggers for all tables in the schema. Tables must already have;
PROMPT metadata in GDOSYS and the sequence must already exist and be assigned.;
EXEC GOOM.DblLine;
ACCEPT tname CHAR PROMPT 'Create Trigger for the following table (ALL) --->' DEFAULT 'ALL';
EXEC GOOM.DotLine;
DECLARE
CURSOR GetSequence IS SELECT TABLE_NAME,COLUMN_NAME,SEQUENCE_NAME 
                        FROM GDOSYS.GFIELDMAPPING 
                       WHERE OWNER=USER 
                         AND SEQUENCE_NAME IS NOT NULL
                         AND TABLE_NAME IN (SELECT TABLE_NAME FROM USER_TABLES);
  c_cmdname   VARCHAR2(32):='CreateSeqTriggers';
  v_sql       VARCHAR2(1024);
  v_seqinfo   GetSequence%ROWTYPE;
  v_trigname  VARCHAR2(30);
  v_tablename VARCHAR2(30):=UPPER('&tname');
  v_seqname   VARCHAR2(30);
  v_pkname    VARCHAR2(30);
  v_count     PLS_INTEGER:=1;
BEGIN
IF v_tablename = 'ALL' THEN
  FOR v_seqinfo IN GetSequence LOOP
    v_tablename := v_seqinfo.TABLE_NAME;
    v_seqname   := v_seqinfo.SEQUENCE_NAME;
    v_pkname    := v_seqinfo.COLUMN_NAME;
    v_trigname  := substr(v_tablename,1,26)||'_TRG';
    v_sql:='CREATE OR REPLACE TRIGGER '||v_trigname||
           ' BEFORE INSERT ON '||v_tablename||
           ' REFERENCING NEW AS NEW FOR EACH ROW 
             DECLARE
             v_sql VARCHAR2(255);
             BEGIN
               IF :NEW.'||v_pkname||' IS NULL THEN 
                 v_sql:=''SELECT '||v_seqname||'.nextval FROM dual''; 
                 EXECUTE IMMEDIATE v_sql INTO :NEW.'||v_pkname||';
               END IF;
             END;';
    --GOOM.DBMSG(v_sql);
    EXECUTE IMMEDIATE v_sql;
    v_count := v_count+1;
  END LOOP;
ELSE
  v_sql:='SELECT SEQUENCE_NAME 
            FROM GDOSYS.GFIELDMAPPING 
           WHERE OWNER=USER 
             AND SEQUENCE_NAME IS NOT NULL 
             AND TABLE_NAME='''||v_tablename||'''';
  EXECUTE IMMEDIATE v_sql into v_seqname;
  v_sql:='SELECT COLUMN_NAME 
            FROM GDOSYS.GFIELDMAPPING 
           WHERE OWNER=USER 
             AND SEQUENCE_NAME='''||v_seqname||''' 
             AND TABLE_NAME='''||v_tablename||'''';
  EXECUTE IMMEDIATE v_sql into v_pkname;
  v_trigname:=substr(v_tablename,1,26)||'_TRG';
  v_sql:='CREATE OR REPLACE TRIGGER '||v_trigname||
         ' BEFORE INSERT ON '||v_tablename||
         ' REFERENCING NEW AS NEW FOR EACH ROW 
           DECLARE
           v_sql VARCHAR2(255);
           BEGIN
             IF :NEW.'||v_pkname||' IS NULL THEN 
               v_sql:=''SELECT '||v_seqname||'.nextval FROM dual''; 
               EXECUTE IMMEDIATE v_sql INTO :NEW.'||v_pkname||';
             END IF;
           END;';
  --GOOM.DBMSG(v_sql);
  EXECUTE IMMEDIATE v_sql;
END IF;
GOOM.DBMSG('Triggers created: '|| v_count);
EXEC GOOM.DblLine;
EXCEPTION
  WHEN OTHERS THEN
    GOOM.REPORT_ERROR (c_cmdname,v_sql,'Tab:'||v_tablename||' Key:'||v_pkname||' Seq:'||v_seqname,sqlcode,sqlerrm);
END;
/
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON