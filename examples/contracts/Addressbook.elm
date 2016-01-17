module Addressbook 
  ( PersonContract
  , AddressBookContract
  , encodePerson
  , decodePerson
  , encodeAddressBook
  , decodeAddressBook ) where

import Native.Addressbook

-- Opaque Type definitions
type PersonContract = Opaque_PersonContract
type AddressBookContract = Opaque_AddressBookContract

decodePerson : Buffer -> PersonContract
decodePerson = Native.Addressbook.decodePerson
decodeAddressBook : Buffer -> AddressBookContract
decodeAddressBook = Native.Addressbook.decodeAddressBook
encodePerson : PersonContract -> Buffer
encodePerson = Native.Addressbook.encodePerson
encodeAddressBook : AddressBookContract -> Buffer
encodeAddressBook = Native.Addressbook.encodeAddressBook
