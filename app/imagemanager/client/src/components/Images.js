// OBP Image Manager, an image-manager for book publishing, by Martin Keegan
//
// Copyright (C) 2019, Open Book Publishers CIC Ltd
//
// This programme is free software; you may redistribute and/or modify
// it under the terms of the Apache Licence v2.0.
import React from 'react'
import List, {
  ListItem,
  ListItemText,
  ListItemGraphic
} from '@material/react-list';
import {
  Body1,
  Body2,
  Button,
  Caption,
  Headline1,
  Headline2,
  Headline3,
  Headline4,
  Headline5,
  Headline6,
  Overline,
  Subtitle1,
  Subtitle2,
} from '@material/react-typography';
import IconButton from '@material/react-icon-button';
import MaterialIcon from '@material/react-material-icon';

import '@material/react-list/dist/list.css';
import '@material/react-typography/dist/typography.css';
import '@material/react-icon-button/dist/icon-button.css';

class Images extends React.Component {
    state = {
        selectedIndex: 1
    };

    handleS(selectedIndex) {
        console.log(this.state.selectedIndex)
        console.log(selectedIndex)
        this.setState({selectedIndex})
    }

    handleDragStart(e, index) {
        console.log(e, index)
    }

    handleDrop(e, index, name) {
        console.log(e, index, name)
    }

    render() {
        return (
          <List twoLine
          >
            {this.props.images.map((img, idx) => {
                const thumbnail_url = "/thumbnails/" + img.id
                const image_url = "/images/" + img.id
                const chapterId = img.figure_id;
                const ord = idx + 1;
                const imgText = img.text;
                const secondary = "Fig " + chapterId + "." + ord + ", " + img.res_category +
                      " (" + img.resolution + ")";

                return (
                    <ListItem
                       key={img.id}
                        >
                      <ListItemGraphic graphic={
                              <a href={image_url} target='_blank'>
                                <img src={thumbnail_url} />
                              </a>} />
                      <IconButton onClick={() => this.props.deleteCb(img.id)}>
                        <MaterialIcon icon='remove_circle_outline' />
                      </IconButton>
                      <IconButton onClick={() => this.props.setRankCb(img, -1)}>
                        <MaterialIcon icon='keyboard_arrow_up' />
                      </IconButton>
                      <IconButton onClick={() => this.props.setRankCb(img, 1)}>
                        <MaterialIcon icon='keyboard_arrow_down' />
                      </IconButton>
                      <IconButton onClick={() => this.props.editCb(img.id, img)}>
                        <MaterialIcon icon='edit' />
                      </IconButton>
                      <ListItemText
                        primaryText={imgText}
                        secondaryText={secondary} />
                    </ListItem>
                )
            }
            )}
          </List>
        )
    }
}

export default Images
