module AddressBook 
  ( Person
  , AddressBook
  , PersonContract
  , AddressBookContract
  , encodePerson
  , decodePerson
  , marshalPerson
  , unmarshalPerson
  , encodeAddressBook
  , decodeAddressBook
  , marshalAddressBook
  , unmarshalAddressBook ) where

import Opaque exposing (Buffer)



import Native.AddressBook


type alias Person = 
  { name : String
  , id : Int
  , email : String
  , phone : List (PhoneNumber) }

type alias AddressBook = 
  { person : List (Person) }

-- Opaque Type definitions
type PersonContract = Opaque_PersonContract
type AddressBookContract = Opaque_AddressBookContract

encodePerson : PersonContract -> Buffer
encodePerson = Native.AddressBook.encodePerson
decodePerson : Buffer -> PersonContract
decodePerson = Native.AddressBook.decodePerson
marshalPerson : Person -> PersonContract
marshalPerson = Native.AddressBook.marshalPerson
unmarshalPerson : PersonContract -> Person
unmarshalPerson = Native.AddressBook.unmarshalPerson
encodeAddressBook : AddressBookContract -> Buffer
encodeAddressBook = Native.AddressBook.encodeAddressBook
decodeAddressBook : Buffer -> AddressBookContract
decodeAddressBook = Native.AddressBook.decodeAddressBook
marshalAddressBook : AddressBook -> AddressBookContract
marshalAddressBook = Native.AddressBook.marshalAddressBook
unmarshalAddressBook : AddressBookContract -> AddressBook
unmarshalAddressBook = Native.AddressBook.unmarshalAddressBook
