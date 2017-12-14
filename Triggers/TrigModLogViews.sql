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
-- This is an example of modifying a MODLOG Trigger to include a view on the base table
-- served by the trigger.   This allows for full notification in GeoMedia when using views.
-- This is current thru GeoMedia 6.1 and requires the GOOM package to be installed first.
--
-- Modlog triggers must be in the form featureclassGMT.  GM will detect the trigger and will
-- off-load modification logging to the database.
-- ----------------------------------------------------------------------------------------
-- Table: TESTAREA View: TESTAREA_VIEW and TESTAREA_VIEW2
--
CREATE OR REPLACE 
TRIGGER TESTAREAGMT AFTER INSERT OR DELETE OR UPDATE ON TESTAREA FOR EACH ROW  
DECLARE  
  v_featureclass    VARCHAR2(30) := 'TESTAREA';
  v_keyname         VARCHAR2(30) := 'PID';
  --
  v_keyvalue        INTEGER      := 0;
  v_ModType         PLS_INTEGER; 
  -- View List:  Ad entry for each view; v_view1, v_view2, v_view3, etc.
  v_view1           VARCHAR2(30) := 'TESTAREA_VIEW';
  v_view2           VARCHAR2(30) := 'TESTAREA_VIEW2';
  --
BEGIN 
    IF INSERTING THEN               -- Logging an Attribute INSERT 
      v_ModType  := 1;              -- Inserting
      v_keyvalue := :new.PID;       -- Get new pkey value
      -- call GOOM procedure that handles the MODLOG update for you.
      GOOM.LogDataModification( v_featureclass, v_keyname, v_keyvalue, v_ModType);
      -- Add line for each view in View List using correct variable. 
      GOOM.LogDataModification( v_view1, v_keyname, v_keyvalue, v_ModType); 
      GOOM.LogDataModification( v_view2, v_keyname, v_keyvalue, v_ModType); 
    ELSIF UPDATING THEN             -- Logging an Attribute UPDATE
      v_ModType  := 2;              -- Updating
      v_keyvalue := :old.PID;       -- Keep existing pkey value
      -- call GOOM procedure that handles the MODLOG update for you.
      GOOM.LogDataModification( v_featureclass, v_keyname, v_keyvalue, v_ModType);
      -- Add line for each view in View List using correct variable.   
      GOOM.LogDataModification( v_view1, v_keyname, v_keyvalue, v_ModType); 
      GOOM.LogDataModification( v_view2, v_keyname, v_keyvalue, v_ModType); 
    ELSE                            -- Logging an Attribute DELETE 
      v_ModType  := 3;              -- Deleteing
      v_keyvalue := :old.PID;       -- Keep existing pkey value
      -- call GOOM procedure that handles the MODLOG update for you.
      GOOM.LogDataModification( v_featureclass, v_keyname, v_keyvalue, v_ModType); 
      -- Add line for each view in View List using correct variable. 
      GOOM.LogDataModification( v_view1, v_keyname, v_keyvalue, v_ModType); 
      GOOM.LogDataModification( v_view2, v_keyname, v_keyvalue, v_ModType); 
    END IF;   
END;
/
--
-- A dummy trigger is required in order to keep GM from creating double entres in the MODIFICATIONLOG table.
-- Note the name of the trigger applies to the view but the trigger fires on the base table and does nothing.
CREATE OR REPLACE TRIGGER TESTAREA_VIEWGMT AFTER INSERT OR DELETE OR UPDATE ON TESTAREA FOR EACH ROW  
BEGIN
  NULL;
END;
/
CREATE OR REPLACE TRIGGER TESTAREA_VIEW2GMT AFTER INSERT OR DELETE OR UPDATE ON TESTAREA FOR EACH ROW  
BEGIN
  NULL;
END;
/
--