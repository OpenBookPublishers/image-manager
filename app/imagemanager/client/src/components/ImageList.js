// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react'
import PropTypes from 'prop-types'
import Echo from './Echo'
import Uploader from './Uploader'
import Chapters from './Chapters'

class ImageList extends React.Component {
  render () {
    return <div>
      <Chapters chapters={this.props.chapters} />
      <Uploader chapters={this.props.chapters} />
      <Echo images={this.props.images} editor={this.props.editor}
            licences={this.props.licences}
            notifications={this.props.notifications} />
    </div>
  }
};

//ImageList.propTypes = {
//  imagess: PropTypes.arrayOf(PropTypes.shape({
//    //id: PropTypes.string.isRequired,
//    completed: PropTypes.bool.isRequired,
//    text: PropTypes.string.isRequired
//  }).isRequired).isRequired,
//}

export default ImageList
