RECONFIG
========

Using telnet to make a manual reconfigure.

* First, connect to xmpp server ::
	
	telnet 125.212.212.52 5222

* Then, start by send a <stream>::
	
	<stream:stream to='htklabs.com' xmlns='jabberd:client' xmlns:stream='http://etherx.jabber.org/streams'>

* Perform an authenticated login::
	
	<iq id='A1' type='set'><query xmlns='jabber:iq:auth'><username>5370861ecdf11b540511ac93</username><resource>telnet</resource><password>3d722de4b662b16697083a7462f82ccdfd446f12f1513dca55628576f442724b78a612d82b23ee38</password></query></iq>

	<presence/>

* And then manual sends an reconfigure to room ::
	
	<iq  id='Reconfig'  to='539590c2d1224b7828357e07@conference.htklabs.com' type='set'><query xmlns='http://jabber.org/protocol/muc#owner'><x xmlns='jabber:x:data' type='submit'><field var='FORM_TYPE'><value>http://jabber.org/protocol/muc#roomconfig</value></field><field type='boolean' label='Make room persistent' var='muc#roomconfig_persistentroom'><value>1</value></field><field type='boolean' label='Make room members-only' var='muc#roomconfig_membersonly'><value>1</value></field><field type='boolean' label='Allow users to change the subject' var='muc#roomconfig_changesubject'><value>0</value></field><field type='boolean' label='Allow users to send private messages' var='allow_private_messages'><value>0</value></field></x></query></iq>

  .. Note::

  	To reconfigure room, user must be room's owner.
