module Addressbook 
  ( PersonContract
  , AddressBookContract
  , encodePerson
  , decodePerson
  , marshalPerson
  , unmarshalPerson
  , encodeAddressBook
  , decodeAddressBook
  , marshalAddressBook
  , unmarshalAddressBook ) where

import Native.Addressbook

-- Opaque Type definitions
type PersonContract = Opaque_PersonContract
type AddressBookContract = Opaque_AddressBookContract

encodePerson : PersonContract -> Buffer
encodePerson = Native.Addressbook.encodePerson
decodePerson : Buffer -> PersonContract
decodePerson = Native.Addressbook.decodePerson
marshalPerson : Person -> PersonContract
marshalPerson = Native.Addressbook.marshalPerson
unmarshalPerson : PersonContract -> Person
unmarshalPerson = Native.Addressbook.unmarshalPerson
encodeAddressBook : AddressBookContract -> Buffer
encodeAddressBook = Native.Addressbook.encodeAddressBook
decodeAddressBook : Buffer -> AddressBookContract
decodeAddressBook = Native.Addressbook.decodeAddressBook
marshalAddressBook : AddressBook -> AddressBookContract
marshalAddressBook = Native.Addressbook.marshalAddressBook
unmarshalAddressBook : AddressBookContract -> AddressBook
unmarshalAddressBook = Native.Addressbook.unmarshalAddressBook
