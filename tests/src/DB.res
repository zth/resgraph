open Belt

module User = {
  type t = {
    id: string,
    name: string,
    age: int,
    lastAge: option<int>, // nullable column
  }

  let rows = [{id: "1", name: "User 1", age: 35, lastAge: None}]

  let find = async id => {
    rows->Array.getBy(row => row.id == id)
  }

  let findMany = async ids => {
    await ids->Array.map(find)->Js.Promise2.all
  }
}

module Group = {
  type t = {
    id: int,
    name: string,
  }

  let rows = [{id: 1, name: "Group A"}]

  let find = async id => {
    rows->Array.getBy(row => row.id == id)
  }

  let findMany = async ids => {
    await ids->Array.map(find)->Js.Promise2.all
  }
}

module Membership = {
  type t = {
    memberId: string,
    groupId: int,
  }

  let rows = [{memberId: "1", groupId: 1}]

  let findGroupMembers = async groupId => {
    rows->Array.keep(row => row.groupId == groupId)->Array.map(row => row.memberId)
  }
}
