--------------------------------------------------------------------------------
-- This module provides a nice API for variables that require additional
-- commands in the Controller.update function to be executed in order to
-- successfully update their values.
--------------------------------------------------------------------------------

module Updatable exposing
  ( Updatable -- do not expose internals
  , create
  , extract
  , needsUpdate
  , setUpdated
  )

--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

type UpdateStatus
  = NeedsUpdate
  | Updated

-- Opaque type, internals hidden from user
type Updatable a = U UpdateStatus a

--------------------------------------------------------------------------------
-- Manipulating values
--------------------------------------------------------------------------------

-- Create an Updatable variable that needs to be updated.
create : a -> Updatable a
create val =
  U NeedsUpdate val

-- Extract a value from an Updatable variable.
extract : Updatable a -> a
extract (U _ a) =
  a

--------------------------------------------------------------------------------
-- Manipulating update statuses
--------------------------------------------------------------------------------

-- Whether or not an Updatable variable needs to be updated.
needsUpdate : Updatable a -> Bool
needsUpdate (U status _) =
  case status of
    NeedsUpdate ->
      True
    Updated ->
      False

-- Mark a variable as updated.
setUpdated : Updatable a -> Updatable a
setUpdated (U _ a) =
  U Updated a
