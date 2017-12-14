-- -------------------------------------------------------------------------------------------------
-- Chuck Woodbury - Senior Systems Consultant, Technology Services.
-- Chuck.Woodbury@HexagonSI.com
-- Hexagon Safety & Infrastructure / Hexagon Geospatial
-- Huntsville, Alabama 35894
-- 256-730-7755
--
-- -------------------------------------------------------------------------------------------------
-- Here is a simple instead of trigger example:

-- Create base table
CREATE TABLE BASETABLE (PID INTEGER PRIMARY KEY, GEOMETRY SDO_GEOMETRY);
CREATE SEQUENCE BASETABLE_SQ;
BEGIN
  FOR I IN 1..5 LOOP
    INSERT INTO BASETABLE VALUES(BASETABLE_SQ.NEXTVAL, GDODATA.Random_PT);
  END LOOP;
END;
/
-- Create secondary table using BT_PID as common column with BASETABLE PID
CREATE TABLE SECONDTABLE(PK_ID INTEGER PRIMARY KEY, BT_PID INTEGER, TESTVALUE VARCHAR2(32));
CREATE SEQUENCE SECONDTABLE_SQ;
BEGIN
  FOR I IN 1..5 LOOP
    INSERT INTO SECONDTABLE VALUES(SECONDTABLE_SQ.NEXTVAL, I, 'CHUCK-'||I);
  END LOOP;
END;
/
--
/*
DROP TABLE BASETABLE;
DROP TABLE SECONDTABLE;
DROP SEQUENCE BASETABLE_SQ;
DROP SEQUENCE SECONDTABLE_SQ;
*/
-- Create Join view
CREATE OR REPLACE VIEW JOINVIEW AS
  SELECT B.PID, B.GEOMETRY, S.TESTVALUE
    FROM BASETABLE B, SECONDTABLE S
   WHERE B.PID=S.BT_PID;
-- Set the GDOSYS metadata for the JOINVIEW
EXEC GOOM.SETGDOSYSMETADATA('JOINVIEW','BASETABLE_SQ','D',10,'PID');
-- SET THE ORACLE METADATA
INSERT INTO USER_SDO_GEOM_METADATA VALUES('JOINVIEW','GEOMETRY',
                 MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT('X',- 2147483648, 2147483648, 0.00005),
                 MDSYS.SDO_DIM_ELEMENT('Y',- 2147483648, 2147483648, 0.00005),
                 MDSYS.SDO_DIM_ELEMENT('Z',- 2147483648, 2147483648, 0.00005)),NULL);
INSERT INTO USER_SDO_GEOM_METADATA VALUES('BASETABLE','GEOMETRY',
                 MDSYS.SDO_DIM_ARRAY(MDSYS.SDO_DIM_ELEMENT('X',- 2147483648, 2147483648, 0.00005),
                 MDSYS.SDO_DIM_ELEMENT('Y',- 2147483648, 2147483648, 0.00005),
                 MDSYS.SDO_DIM_ELEMENT('Z',- 2147483648, 2147483648, 0.00005)),NULL);

COMMIT;
-- CREATE THE INSTEAD OF TRIGGER
CREATE OR REPLACE TRIGGER JOINVIEW_TRG 
INSTEAD OF INSERT OR UPDATE OR DELETE ON JOINVIEW 
BEGIN
  IF INSERTING THEN
    INSERT INTO BASETABLE(PID, GEOMETRY) VALUES (:NEW.PID, :NEW.GEOMETRY);
    INSERT INTO SECONDTABLE(PK_ID, BT_PID, TESTVALUE) VALUES (SECONDTABLE_SQ.NEXTVAL,:NEW.PID,:NEW.TESTVALUE);
  ELSIF UPDATING THEN
    UPDATE BASETABLE SET GEOMETRY=:NEW.GEOMETRY WHERE PID=:OLD.PID;
    UPDATE SECONDTABLE SET TESTVALUE=:NEW.TESTVALUE WHERE BT_PID=:OLD.PID;
  ELSE
    DELETE FROM BASETABLE WHERE PID=:OLD.PID;
    DELETE FROM SECONDTABLE WHERE BT_PID=:OLD.PID;
  END IF;
END;
/
-- Done
