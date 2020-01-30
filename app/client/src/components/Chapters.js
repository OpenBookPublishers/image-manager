// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react';
import { connect } from 'react-redux'
import { setChapter } from '../actions'
import Select from '@material/react-select';

import "@material/react-list/dist/list.css";
import "@material/react-menu-surface/dist/menu-surface.css";
import "@material/react-menu/dist/menu.css";
import "@material/react-select/dist/select.css";

class Chapters extends React.Component {
  constructor(props) {
    super(props)
    this.dispatch = props.dispatch;
  }

  changed(index, target) {
    const dispatcher = setChapter;
    this.dispatch(dispatcher(target.getAttribute('data-value')));
  }

  render() {
    return (
      <Select raised enhanced
        label='Select chapter'
        onEnhancedChange={(index, target) => this.changed(index, target)}
        value={this.props.chapters.current}
        options={this.props.chapters.chapter_names}
      />
    );
  }
}

export default connect()(Chapters);
