// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react';
import { connect } from 'react-redux';
import {Snackbar} from '@material/react-snackbar';
import { closeNotifications } from '../actions'

import '@material/react-snackbar/dist/snackbar.css';

class Notifications extends React.Component {
  constructor(props) {
    super(props)
    this.dispatch = props.dispatch
  }

  onCloseCb(action) {
    this.dispatch(closeNotifications())
  }

  render() {
    return (
      <Snackbar stacked
        open={this.props.notifications.isOpen}
        message={this.props.notifications.message}
        onClose={(action) => this.onCloseCb(action)}
      />
    );
  }
}

export default connect()(Notifications);
