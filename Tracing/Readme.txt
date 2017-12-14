These are DBA only procedures.
==============================
The GOOM Package is requried, load it as DBA, it will install into GDOSYS.
As SYSTEM, run the DBTraceSessionProc.sql, this is the trace procedure.  
Connect as SYS and grant execute on DBMS_MONITOR to SYSTEM.
Run SET SERVEROUTPUT ON
To trace a session, run DBTraceSession.sql as SYSTEM and answer the prompts. 
To trace a connection, run DBTraceConnect.sql and provide the schema name.  This will create a trigger that will start a trace on connection to that schema.

Send the raw trace files to Support.