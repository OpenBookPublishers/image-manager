// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import { combineReducers } from 'redux'
import images from './images'
import visibilityFilter from './visibilityFilter'
import chapters from './chapters'
import editor from './editor'
import licences from './licences'
import notifications from './notifications'

export default combineReducers({
  images,
  visibilityFilter,
  chapters,
  editor,
  licences,
  notifications
})
