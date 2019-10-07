OBP Image Manager, minimum viable implementation
================================================

Author: Martin Keegan

Date: 2019-09-17

Last updated: 2019-09-29

Distribution: OBP internal

Background
----------

We are creating a small web-based tool for saving effort involved in
managing the collection of images to be used in books. It is likely that
there is some minimum threshold, e.g., 20 images, beyond which such a tool
could reduce the effort expended. It is also intended to make things easier
for authors such that some of the effort can be offloded onto the author
him/herself.

A tool like this was identified by production staff as the preferred
priority for workflow automation at the off-site day-long meeting held
with Adam Hyde of Coko / Editoria / Book Sprints in 2019Q1.

Process
-------

There will be a process of consultation with all affected production staff
leading to updates to this document reflecting whatever is agreed. In
parallel, software will be developed to reflect the agreed specification.

It is unlikely that specifications conceived in the abstract will actually
reflect what we need. Therefore after the initial implementation the
process will be repeated once or twice to make best use of hindsight and
experience.

Specification
=============

The system shall provide a web inteface permitting the author and editors
to do the following:

* upload images
* list the available images
* edit caption and copyright status
* re-order the images to reflect their order in the book
* automatically (re)-generate sequence numbers for the images
* delete images
* add chapters (and possibly their names)

The system should itself automatically:

* determine the resolution of the images
* determine the format of the images
* prevent the upload of invalid material
* generate a ``lint'' report about known problems with the current set of images
* create a downloadable archive of the current set of images, e.g., a ZIP file

Image formats
-------------

The only image formats to be supported shall be those in the Author's Guide:

* JPEG
* TIFF
* PNG
* PDF

The system shall reject attempts to upload material that cannot be
automatically determined to be in these formats.

Resolution
----------

Each image shall be automatically determined to belong to one and only one
of the following categories, lifted directly from the Authors' Guide:

* Full-page images, including the cover image, should be approx. 3200 x 2500
pixels.
* Half-page images, which should be at least 2100 x 1700 pixels.
* Smaller images, which should be at least 800 x 500 pixels.
* Images that are to be rejected for being too small

Copyright status
----------------

The system shall enforce the requirement that every image be accompanied by
one and only one copyright status, which may be updated by the user and must
be from the following list:

* To Be Determined
* Public Domain
* All Rights Reserved
* CC-BY
* CC-BY-SA
* Fair use

This list presumably is incomplete and needs a few more isotopes of
the Creative Commons licence.

Data model
----------

For a given book, we shall record the following:

* a set of images
* the number of chapters in the book
* the ordering of these images in the book, by chapter
* placeholders reflecting the absence or deletion of an image, or that
  we have no appropriate licence for the image and thus must provide a URL
  instead

For a given image, we shall mandatorily record the following:

* the filename of the image
* the caption associated with the image
* the licence information about the image (this may need to be expanded to
  include a lot of material about provenance / proof of licence etc)
* the content of the image itself
* resolution (see above)
* format (see above)
* whether the image is a placeholder

Optionally we shall also record the following in respect of each image:

* a URL where a copy of the image might be obtained
* the artist who made the original work
* the title of the original work
* the year of the original work
* the medium in which the original work was created
* the original size of the work

Finally the sequence identifiers relating the images to their position in 
the book shall be recorded (e.g., such-and-such an image is in the book
at position "3.4", i.e., the fourth image in chapter 3).

Images shall be related to chapters in the following way:

* zero, one or more images shall be related to an image_set (in an order?)
* multiple image_sets, in order, shall be related to a chapter

Placeholders
------------

It shall be possible for the system to be used to track material which
isn't going to be included in the distributed copy of the book, but which
should still receive a number in the numbering scheme. This may be because
no image of sufficient resolution exists, or because no freely-destributable
image exists. Each image shall be flaggable as as a placeholder, and this
shall relax the checks performed when determining image status (see below).

Numbering schemes
-----------------

Initially we shall support the most popular image sequence numbering scheme, the
``by chapters'' scheme:

* such-and-such an image is in the book at position "3.4", i.e., the fourth image in chapter 3)

The other two common schemes are:

* no numbering scheme
* numbering all the images in book regardless of chapter

Initially these can be supported just by ignoring all or part of the
sequence number.

There shall be a facility for explicitly initiating an automatic renumbering
of the images. It is currently intended that we *not* permit manual
renumbering, whereby editors would type in particular sequence numbers
against images.

Notifications
-------------

There are certain events which should give rise to a notification to be
sent to the user:

* upload of something which isn't an image
* upload of an image that already exists

Authors should be notified to renumber their captions when the system
becomes aware that this might be necessary.

The system shall unobtrusively inform the author how many image upload
are currently in progress.

Image status
------------

Each image shall have an automatically-determined status, which shall
be available to the end-user in some fashion. The status shall be one
and only one of the following:

* accepted
* resolution too low
* licence required
* no appropriate caption

This list may be expanded in the light of hindsight, with a view to
getting the author to do more of the work and enabling editors to
survey progress efficiently.

Out of scope
------------

The following can be added later, and won't be considered for the present
effort:

* Usernames / passwords / authentication / user roles
* Managing multiple books
* Anything other than images, e.g., audio or video files
* Image sets which share a sequence number but which have one caption per image
* Export of caption information
* Uploading images without associating them with a chapter
* Editing chapter names / changing the number of chapters
* Recording / editing the optional image metadata
* Tracking of number of in-flight image uploads
