// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React, {Component} from 'react';
import { connect } from 'react-redux'
import Dialog, {
  DialogTitle,
  DialogContent,
  DialogFooter,
  DialogButton,
} from '@material/react-dialog';
import TextField, {HelperText, Input} from '@material/react-text-field';
import Select from '@material/react-select';
import { closeEditor, updateEditor } from '../actions'

import "@material/react-dialog/dist/dialog.css";
import '@material/react-text-field/dist/text-field.css';

import "@material/react-list/dist/list.css";
import "@material/react-menu-surface/dist/menu-surface.css";
import "@material/react-menu/dist/menu.css";
import "@material/react-select/dist/select.css";


class EditImage extends Component {
  constructor(props) {
    super(props)
    this.dispatch = props.dispatch
  }

  close(action) {
    switch(action) {
      case "save":
        console.log(this.props.editor)
        this.props.saveCb(this.props.editor.hash, this.props.editor.image)
        this.dispatch(closeEditor())
        break;
      case "cancel":
        this.dispatch(closeEditor())
        break;
      default:
        console.log("unrecognised action: " + action)
    }
  }

  update(key, e) {
    this.dispatch(updateEditor(key, e.currentTarget.value));
  }

  licenceChanged(index, target) {
    console.log(target.getAttribute('data-value'));
    this.dispatch(updateEditor('licence_status',
                               target.getAttribute('data-value')));
  }

  render() {

// thumbnail
// caption
// licence
// alt URL
// artist
// title
// year
// medium
// orig size
    return (
      <Dialog
        onClose={(action: string) => this.close(action)}
        open={this.props.editor.isOpen}>
        <DialogTitle>
            Image details
        </DialogTitle>
        <DialogContent>
          <TextField
            label='Image caption'
          ><Input
             value={this.props.editor.image.text}
             onChange={(e) => this.update('text', e)} />
          </TextField>

          <Select raised enhanced
            label='Copyright status'
            onEnhancedChange={(index, target) =>
                              this.licenceChanged(index, target)}
            value={this.props.editor.image.licence_status}
            options={this.props.licences} />

        </DialogContent>
        <DialogFooter>
          <DialogButton action='cancel'>Cancel</DialogButton>
          <DialogButton raised action='save' isDefault>Save</DialogButton>
        </DialogFooter>
      </Dialog>
    );
  }
}

export default connect()(EditImage);
