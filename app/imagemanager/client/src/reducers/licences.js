// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.

const defaults = () => {
    return [
        {
            label: "To Be Determined",
            value: "To Be Determined"
        }
    ];
}

const makeLicence = (detail) => {
    return {
        label: detail.licence_status,
        value: detail.licence_status,
        allowed: detail.allowed
    }
}

const licences = (state = defaults(), action) => {
  switch(action.type) {
    case 'GET_ALL_LICENCES':
      if(action.details === "") {
        action.details = [] // sic
      }
      return action.details.map(detail => makeLicence(detail))
    default:
      return state
  }
}

export default licences;
