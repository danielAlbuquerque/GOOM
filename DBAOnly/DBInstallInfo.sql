-- Verify Status of Spatial/Locator Components
COLUMN COMP_ID FORMAT A8;
COLUMN SCHEMA FORMAT A8;
COLUMN VERSION FORMAT A12;
COLUMN STATUS FORMAT A8;
COLUMN COMP_NAME FORMAT A45;
COLUMN GRANTOR FORMAT A10 ;
COLUMN GRANTEE FORMAT A10 ;
COLUMN PRIVILEGE FORMAT A30 ;
COLUMN ADMIN_OPTION FORMAT A5 ;
COLUMN OBJECT_NAME FORMAT A60;
COLUMN OBJECT_TYPE FORMAT A20;
--
Select * from v$Version;
--
PROMPT Package Versions ;
SELECT COMP_ID, SCHEMA, VERSION, STATUS, COMP_NAME   
  FROM dba_registry
 WHERE COMP_ID NOT IN ('APS','XOQ','AMD','APEX'); 
-- 
PROMPT Possible Invalid Objects? ;
SELECT owner,object_name, object_type, status
  FROM all_objects 
 WHERE status <> 'VALID' AND OBJECT_NAME NOT LIKE 'BIN$%'
 ORDER by object_name;
--
     