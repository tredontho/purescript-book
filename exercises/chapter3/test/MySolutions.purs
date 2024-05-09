module Test.MySolutions where

import Prelude
import Data.List (filter, head, nubByEq)
import Data.Maybe (Maybe(..))

import Data.AddressBook (AddressBook, Entry, findEntry)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet name = head <<< filter (_.address.street >>> (_ == name))

isInBook :: String -> String -> AddressBook -> Boolean
isInBook first last book = findEntry first last book /= Nothing

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates x = nubByEq (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName) x
