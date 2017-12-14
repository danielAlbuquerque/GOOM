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
-- --------------------------------------------------------------------------------------------------
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
-- @ListMetadata
-- List the metadata assigned to a table.  The information comes from GDOSYS.
-- ----------------------------------------------------------------------------------------
-- History: 07/02/2005  Created.
--          04/17/2007  Modified.
--          04/26/2010  Modified to use owner.table and binds.
-- ----------------------------------------------------------------------------------------
PROMPT ==============================================================================;
PROMPT List GDOSYS Metadata for the specified feature class.;
PROMPT ==============================================================================;
ACCEPT tabname CHAR PROMPT  'Enter the table (or owner.table) -->';
--
SET TERMOUT OFF
SET VERIFY OFF
SET TIMING OFF
VARIABLE vownertable VARCHAR2(61)
VARIABLE vowner VARCHAR2(30)
VARIABLE vtable VARCHAR2(30)
EXEC :vownertable := GOOM.GetOwnerObject(UPPER('&tabname'));
EXEC :vowner      := GOOM.SplitOwnerObject(:vownertable,'OWNER');
EXEC :vtable      := GOOM.SplitOwnerObject(:vownertable,'TABLE');
SET TERMOUT ON
--
COLUMN GEOMETRY FORMAT A24;
COLUMN ST FORMAT 99;
COLUMN SEQUENCE FORMAT A30;
COLUMN SEQOWNER FORMAT A25;
COLUMN COLUMN_NAME FORMAT A30;
--
COLUMN CS_NAME FORMAT A24;
COLUMN GTYPE FORMAT 99;
COLUMN P_GEOM FORMAT A24;
--
COLUMN CS_NAME FORMAT A24;
COLUMN CSGUID FORMAT A42;
COLUMN GEOM_COLUMN FORMAT A24;
COLUMN FEATURE_CLASS FORMAT A24;
COLUMN FIELDNAME FORMAT A32;
COLUMN FIELDFORMAT FORMAT A15;
COLUMN FT FORMAT 99;
COLUMN FD FORMAT 99;
COLUMN FP FORMAT 99;
COLUMN PF FORMAT 99;
--
PROMPT ==============================================================================;
PROMPT Named coordinate systems used by &&tabname;
PROMPT ..............................................................................;
SELECT A.NAME CS_NAME,B.COLUMN_NAME GEOM_COLUMN, A.CSGUID 
  FROM GDOSYS.GCOORDSYSTEM A, GDOSYS.GFIELDMAPPING B
 WHERE B.OWNER IN (SELECT :vowner FROM DUAL) 
   AND B.TABLE_NAME=:vtable
   AND A.CSGUID=B.CSGUID;
--
PROMPT ==============================================================================;
PROMPT GFEATURES entries for &tabname;
PROMPT ..............................................................................;
SELECT GEOMETRYTYPE GTYPE,
       PRIMARYGEOMETRYFIELDNAME P_GEOM
  FROM GDOSYS.GFEATURES
 WHERE FEATURENAME IN (SELECT :vownertable from DUAL);
--
PROMPT ==============================================================================;
PROMPT GFIELDMAPPING entries for &tabname;
PROMPT ..............................................................................;
SELECT A.COLUMN_NAME,
       A.DATA_TYPE FT,
       A.DATA_SUBTYPE ST,
       A.SEQUENCE_OWNER SEQOWNER,
       A.SEQUENCE_NAME SEQUENCE,
       A.CSGUID
  FROM GDOSYS.GFIELDMAPPING A
 WHERE A.OWNER IN (SELECT :vowner FROM DUAL) and A.TABLE_NAME=:vtable;
--
PROMPT ==============================================================================;
PROMPT ATTRIBUTEPROPERTIES entries for &&tabname;
PROMPT ..............................................................................;
SELECT A.INDEXID, B.FIELDNAME,A.FIELDFORMAT,A.FIELDTYPE FT ,A.ISFIELDDISPLAYABLE FD,A.FIELDPRECISION FP
  FROM GDOSYS.ATTRIBUTEPROPERTIES A, GDOSYS.FIELDLOOKUP B
 WHERE A.INDEXID=B.INDEXID 
   AND B.INDEXID IN (SELECT INDEXID
                       FROM GDOSYS.FIELDLOOKUP
                      WHERE FEATURENAME IN (SELECT :vownertable from DUAL));
PROMPT ==============================================================================;
PROMPT GEOMETRYPROPERTIES entries for &&tabname;
PROMPT ..............................................................................;
SELECT A.INDEXID, B.FIELDNAME ,A.PRIMARYGEOMETRYFLAG PF,A.GEOMETRYTYPE GTYPE ,A.GCOORDSYSTEMGUID CSGUID
  FROM GDOSYS.GEOMETRYPROPERTIES A, GDOSYS.FIELDLOOKUP B
 WHERE A.INDEXID=B.INDEXID and B.INDEXID
    IN (SELECT INDEXID
        FROM GDOSYS.FIELDLOOKUP
        WHERE FEATURENAME IN (SELECT :vownertable from DUAL));
PROMPT ==============================================================================;
PROMPT GINDEX_COLUMNS entries for &&tabname;
PROMPT ..............................................................................;
SELECT OBJECT_NAME,INDEX_NAME ,INDEX_TYPE TYP, COLUMN_NAME
  FROM GDOSYS.GINDEX_COLUMNS
 WHERE OWNER=:vowner AND OBJECT_NAME= :vtable;
--
SET TIMING ON
/*
PROMPT ==============================================================================;
PROMPT USER_SDO_GEOM_METADATA for &&tabname;
PROMPT ..............................................................................;
SELECT A.COLUMN_NAME, A.DIMINFO
  FROM USER_SDO_GEOM_METADATA A
 WHERE A.TABLE_NAME = UPPER('&&tabname');
--
PROMPT ==============================================================================;
PROMPT FIELDLOOKUP metadata entries used by &&tabname;
PROMPT ..............................................................................;
SELECT FIELDNAME, INDEXID
  FROM GDOSYS.FIELDLOOKUP
 WHERE FEATURENAME IN (SELECT USER||'.&&tabname' from DUAL);
*/
-- ----------------------------------------------------------------------------------------