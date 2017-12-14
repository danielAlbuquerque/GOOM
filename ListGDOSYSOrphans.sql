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
-- --------------------------------------------------------------------------------------------------- DISCLAIMER
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
-- @ListGDOSYSOrphans
-- 
-- List any orphan metadata in the current user.
-- ----------------------------------------------------------------------------------------
SET VERIFY OFF
SET TIMING OFF
SET FEEDBACK OFF
EXEC GOOM.DBLLine;
EXEC GOOM.TITLELINE('Orphans in GFEATURES');
EXEC GOOM.DotLine;
SELECT featurename FROM GDOSYS.GFEATURESBASE 
        WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,'.',1)+1,LENGTH(FEATURENAME)) 
       NOT IN (SELECT object_name FROM USER_OBJECTS)
          AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,'.',1)-1) 
           IN (SELECT USER FROM DUAL);
--
EXEC GOOM.TITLELINE('Orphans in GFIELDMAPPING');
EXEC GOOM.DotLine;
SELECT table_name FROM GDOSYS.GFIELDMAPPING 
 WHERE OWNER IN (SELECT USER FROM DUAL)
   AND TABLE_NAME NOT IN (SELECT object_name FROM USER_OBJECTS);
--
EXEC GOOM.TITLELINE('Orphans in FIELDLOOKUP');
EXEC GOOM.DotLine;
SELECT featurename FROM GDOSYS.FIELDLOOKUP
 WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,'.',1)+1,LENGTH(FEATURENAME)) 
NOT IN (SELECT object_name FROM USER_OBJECTS)
   AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,'.',1)-1) 
    IN (SELECT USER FROM DUAL);
--
EXEC GOOM.TITLELINE('Orphans in ATTRIBUTEPROPERTIES');
EXEC GOOM.DotLine;
select INDEXID FROM GDOSYS.ATTRIBUTEPROPERTIES
 WHERE INDEXID IN (SELECT indexid FROM GDOSYS.FIELDLOOKUP
 WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,'.',1)+1,LENGTH(FEATURENAME)) 
NOT IN (SELECT object_name FROM USER_OBJECTS)
   AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,'.',1)-1) 
    IN (SELECT USER FROM DUAL));
--
EXEC GOOM.TITLELINE('Orphans in GEOMETRYPROPERTIES');
EXEC GOOM.DotLine;
select INDEXID FROM GDOSYS.GEOMETRYPROPERTIES
 WHERE INDEXID IN (SELECT indexid FROM GDOSYS.FIELDLOOKUP
 WHERE SUBSTR(FEATURENAME,INSTR(FEATURENAME,'.',1)+1,LENGTH(FEATURENAME)) 
NOT IN (SELECT object_name FROM USER_OBJECTS)
   AND SUBSTR(FEATURENAME,1,INSTR(FEATURENAME,'.',1)-1) 
    IN (SELECT USER FROM DUAL));
--
EXEC GOOM.TITLELINE('Orphans in GINDEX_COLUMNS (Views Only)');
EXEC GOOM.DotLine;
select OBJECT_NAME FROM GDOSYS.GINDEX_COLUMNS
 WHERE OBJECT_NAME 
NOT IN (SELECT object_name FROM USER_OBJECTS)
   AND owner  IN (SELECT USER FROM DUAL);
--
EXEC GOOM.DBLLine;
SET TIMING ON
SET FEEDBACK on
SET VERIFY ON