LOG CHAT
========

To store and retrieve log chat for both logchat (client to client) and muc, we will 
apply `XEP 0313`_ - Message Archive Management to our stanza rules.

.. Note::

   - 'localhost' in this document can be replaced by suitable server host.
   - Time must be UTC and adhere to DateTime format specified in `XEP-0082`_ - XMPP Date and Time Profiles


.. _XEP 0313: http://xmpp.org/extensions/xep-0313.html
.. _XEP-0082: http://xmpp.org/extensions/xep-0082.html

C2C
---

1. Archiving Messages

* David sends a message to Michael::

	<message to='michale@localhost' type='chat'>                        
		<body>Hi, it's me, David</body>                                         
	</message>                                                                  

 Server will include element <archived/> to message to inform that message is stored. An unique uid
 is assigned to message. In that time,  Michael will receive the following messages::

	<message from='david@localhost/resource' to='michale@localhost' type='chat'>  
		<body>Hi, it's me, David</body>										   
		<archived by='michale@localhost' id='28482-98726-73623'/>                  
	</message>                                                                                               
    
* David send a file to Michael::

	<message to='michale@localhost' type='chat'>                        
		<body>
			<file>
				<name>file name</name>
				<size>file size</size>
				<fileUrl>file url</fileUrl>
				<mimeType>jpg</imeType>
				<thumbnailUrl>thumb url<thumbnailUrl>
			</file>
		</body>                                         
	</message>    

 Server do the same things like normal chat, Michael will receive the message::

	<message from='david@localhost/resource' to='michale@localhost' type='chat'>                        
		<body>
			<file>
				<name>file name</name>
				<size>file size</size>
				<fileUrl>file url</fileUrl>
				<mimeType>jpg</imeType>
				<thumbnailUrl>thumb url<thumbnailUrl>
			</file>
		</body>   
		<archived by='michale@localhost' id='28482-98726-73890'/>                                       
	</message> 

 .. note::

   	Attribute id :  the message's uid and unique.

2. Query The Archive

   To query the archive message, we must follow `XEP-0059`_ - Result Set Management (RSM)
   to limit the result by paging.

.. _XEP-0059: http://xmpp.org/extensions/xep-0059.html

* First Request.

	* David query the messages before a certain point::

		<iq type='get' id='A1' to='logchat.localhost'>
			<query xmlns='urn:xmpp:mam:tmp' queryid='Q1'/>
				<with>michael@localhost</with>
				<start>2010-08-07T13:23:54Z</end>
				<set xmlns='http://jabber.org/protocol/rsm'>
					<max>50</max>
				</set>
			</query>
	 	</iq>

	 .. Note::  

		- <start/> element is used to include the previous messages to a certain point in time. If <start/> element is omitted, default recent time is considered as start time.
		- <with/> element is used to just query the communication with contact 'michael'. If <with/> is omitted, default query all communication of user 'david' correspond to time query.
 	 	- Include <set/> element, server will return a limited result assigned in <max/> element.

 	* David query the messages after a certain point::

	    <iq type='get' id='A4' to='logchat.localhost'>
			<query xmlns='urn:xmpp:mam:tmp' queryid='f27'/>
				<with>michael@localhost</with>
				<end>2010-07-07T13:23:54Z</end>
				<set xmlns='http://jabber.org/protocol/rsm'>
					<max>50</max>
				</set>
			</query>
		</iq>

	* David query all messages in a certain timespan::

		<iq type='get' id='A4' to='logchat.localhost'>
			<query xmlns='urn:xmpp:mam:tmp' queryid='f27'/>
				<with>michael@localhost</with>
				<start>2010-07-07T13:23:54Z</end>
				<end>2010-08-07T13:23:54Z</end>
				<set xmlns='http://jabber.org/protocol/rsm'>
					<max>50</max>
				</set>
			</query>
		</iq>

	* David query the messages in the last pages::

		<iq type='get' id='A4' to='logchat.localhost'>
			<with>michael@localhost</with>
			<query xmlns='urn:xmpp:mam:tmp' queryid='f27'/>
				<set xmlns='http://jabber.org/protocol/rsm'>
					<max>50</max>
					<before/>
				</set>
			</query>
		</iq>

 	* Server Response.

 	  Firstly, server will send the limited number of matching messages which is considered as first page result 
 	  with the following forms::

		<message id='B1' from='logchat.localhost' to='david@localhost/resource'>
			<result xmlns='urn:xmpp:mam:tmp' queryid='Q1' id='28482-98726-73623'>
				<forwarded xmlns='urn:xmpp:forward:0'>
					<delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:08:25Z'/>
					<message from='david@localhost/resource' to='michale@localhost/resource' type='chat'>
						<body>Hi, it's me, David</body>
					</message>
				</forwarded>
			</result>
		</message>

		<message id='B2' from='logchat.localhost' to='david@localhost/resource'>
			<result xmlns='urn:xmpp:mam:tmp' queryid='Q1' id='28482-98726-73890'>
				<forwarded xmlns='urn:xmpp:forward:0'>
					<delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:09:32Z'/>
					<message from='david@localhost/resource' to='michale@localhost/resource' type='chat'>
						<body>
							<file>
								<name>file name</name>
								<size>file size</size>
								<fileUrl>file url</fileUrl>
								<mimeType>jpg</imeType>
								<thumbnailUrl>thumb url<thumbnailUrl>
							</file>
						</body>
					</message>
				</forwarded>
			</result>
		</message>

	 After finishing sending all matching messages, to ensure the result are complete server will send <iq/> result::

		<iq type='result' id='A1' from='logchat.localhost' to='david@localhost/resource'>
			<query xmlns='urn:xmpp:mam:tmp' queryid='Q1'>
				<with>michael@localhost</with>
				<start>2010-06-07T00:00:00Z</start>
				<end>2010-07-07T05:03:27Z</end>
				<set xmlns='http://jabber.org/protocol/rsm'>
					<first index='0'>28482-98726-73623</first>
					<last>28482-98726-80012</last>
					<count>1000</count>
				</set>
			</query>
		</iq> 

	 .. Note::

	    - <first> and <last> elements specific the UID of the first and last returned result.
	    - <count> element includes number of messages in the full set.
	    - Attribute 'stamp' of <delay> contains original timestamp of the message.
	    - Attribute 'index' should be 0 if it is the first of returned result of full set.
	    - Attribute 'queryid' of matching message result should be the same with client query request.
	    - Attribute 'id' of <iq/ type='result'> should be the same with <iq/ type='get'>. 

* Request Through Result.

	  Client resends the same request include <after/> element and receive the next page of result::
			
			<iq type='get' id='A2' to='logchat.localhost'>
				<query xmlns='urn:xmpp:mam:tmp' queryid='Q2'/>
					<with>michael@localhost</with>
					<start>2010-08-07T13:23:54Z</end>
					<set xmlns='http://jabber.org/protocol/rsm'>
						<max>50</max>
						<after>28482-98726-80012</after>
					</set>
				</query>
	 		</iq>

	  .. Note::
	        
	     Request should contain an <after/> with the UID of the last message it received from the previous query.

	  Server Response::

			[...... server sends matching messages.....]
			<iq type='result' id='A2' from='logchat.localhost' to='david@localhost/resource'>
				<query xmlns='urn:xmpp:mam:tmp' queryid='Q2'>
					<with>michael@localhost</with>
					<start>2010-07-07T05:03:27Z</start>
					<end>2010-07-07T10:03:27Z</end>
					<set xmlns='http://jabber.org/protocol/rsm'>
						<first index='50'>28482-98726-80012</first>
						<last>28482-98726-90043</last>
						<count>1000</count>
					</set>
				</query>
			</iq>  

	  Client resends same request include <before/> element and receive the previous page of result::

			<iq type='get' id='A3' to='logchat.localhost'>
				<query xmlns='urn:xmpp:mam:tmp' queryid='Q3'>
					<with>michael@localhost</with>
					<start>2010-08-07T13:23:54Z</end>
					<set xmlns='http://jabber.org/protocol/rsm'>
						<max>50</max>
						<before>28482-98726-80012</after>
					</set>
				</query>
			</iq>

	  Server Response::
			
			[...... server sends matching messages.....]
			<iq type='result' id='A3' from='logchat.localhost' to='david@localhost/resource'>
				<query xmlns='urn:xmpp:mam:tmp' queryid='Q3'>
					<with>michael@localhost</with>
					<start>2010-07-07T05:03:27Z</start>
					<end>2010-07-07T10:03:27Z</end>
					<set xmlns='http://jabber.org/protocol/rsm'>
						<first index='0'>28482-98726-73623</first>
						<last>28482-98726-80012</last>
						<count>1000</count>
					</set>
				/query>
			</iq>

* Return Empty Page::

	<iq type='result' id='A3' from='logchat.localhost' to='david@localhost/resource'>
		<query xmlns='urn:xmpp:mam:tmp' queryid='Q3'>
			<set xmlns='http://jabber.org/protocol/rsm'>
				<count>1000</count>
			</set>
		</query>
	</iq>

 .. Note::

 	<count> contains number of messages in full set.

* Return Page-Not-Found::

	<iq type='error' id='A3' from='logchat.localhost' to='david@localhost/resource'>
		<query xmlns='urn:xmpp:mam:tmp' queryid='Q3'>
			<start>2010-08-07T13:23:54Z</end>
			<set xmlns='http://jabber.org/protocol/rsm'>
				<max>50</max>
				<before>28482-98726-80012</after>
			</set>
		</query>
		<error type='cancel'>
 			<item-not-found xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
 		</error>
	</iq>

 .. Note::

 	UID in the <after/> or <before/> no longer exists.

* Return <error/> element when client no specific a limit using RSM::

	<iq type='error' id='A3' from='logchat.localhost' to='david@localhost/resource'>
		<error type='modify'>
			<policy-violation xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
			<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>Too many results</text>
		</error>
	</iq>

MUC
---

1. Archiving Messages

* Occupant David sends a message to all occupants::

	<message from='david@localhost/resource' id='A1' to='roomid@conference.localhost' type='groupchat'>
	  <body>Harpier cries: 'tis time, 'tis time.</body>
	</message>

* Beside message is reflected to all occupants follow the rules of `XEP-0045#message`_  (default of muc), 'david' will receive the following messages to confirm that message is stored::

	<message id='A1' from='roomid@conference.localhost' to='david@localhost/resource' type='groupchat'>  
		<body>Harpier cries: 'tis time, 'tis time.</body>										   
		<archived by='roomid@conference.localhost' id='28482-34567-12345'/>                  
	</message>

 .. _XEP-0045#message: http://xmpp.org/extensions/xep-0045.html#message

* Occupant David sends a file to all occupants::
	
	<message from='david@localhost/resource' id='A1' to='roomid@conference.localhost' type='groupchat'>
	  	<body>
	  		<file>
				<name>file name</name>
				<size>file size</size>
				<fileUrl>file url</fileUrl>
				<mimeType>jpg</imeType>
				<thumbnailUrl>thumb url<thumbnailUrl>
			</file>
	  	</body>
	</message>

* Server response to david to confirm file archieved::

	<message from='roomid@conference.localhost' id='A1' to='david@localhost/resource' type='groupchat'>
	  	<body>
	  		<file>
				<name>file name</name>
				<size>file size</size>
				<fileUrl>file url</fileUrl>
				<mimeType>jpg</imeType>
				<thumbnailUrl>thumb url<thumbnailUrl>
			</file>
	  	</body>
	  	<archived by='roomid@conference.localhost' id='28482-34567-12567'/>     
	</message>

* Each time occupand 'david' is added or removed from room, server will auto send an message to report that event is stored::

	<message from='roomid@conference.localhost' id='A1' to='david@localhost/resource' type='groupchat'>
		<body>
			<event>joined</event>
		</body>
		<archived by='roomid@conference.localhost' id='28482-34567-12908'/>     
	</message>

 .. Note::

 	<event> : contain {'joined', 'left'}.

2. Query The Archive

   The same rule with c2c query log chat. Some difference as follows::

		- To query log chat of muc, client make a request to : " to='muclogchat.localhost' ".
		- <with/> element must contain bare jid of room. 

   With Muc query, server will response the matching messages::

   		<message id='B1' from='muclogchat.localhost' to='david@localhost/resource'>
			<result xmlns='urn:xmpp:mam:tmp' queryid='Q1' id='28482-98726-73623'>
				<forwarded xmlns='urn:xmpp:forward:0'>
					<delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:08:25Z'/>
					<message from='david@localhost/resource' to='roomid@conference.localhost' type='groupchat'>
						<body>Harpier cries: 'tis time, 'tis time.</body>
					</message>
				</forwarded>
			</result>
		</message>

		<message id='B2' from='muclogchat.localhost' to='david@localhost/resource'>
			<result xmlns='urn:xmpp:mam:tmp' queryid='Q1' id='28482-98726-73890'>
				<forwarded xmlns='urn:xmpp:forward:0'>
					<delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:09:32Z'/>
					<message from='david@localhost/resource' to='roomid@conference.localhost' type='groupchat'>
						<body>
							<file>
								<name>file name</name>
								<size>file size</size>
								<fileUrl>file url</fileUrl>
								<mimeType>jpg</imeType>
								<thumbnailUrl>thumb url<thumbnailUrl>
							</file>
						</body>
					</message>
				</forwarded>
			</result>
		</message>

		<message id='B3' from='muclogchat.localhost' to='david@localhost/resource'>
			<result xmlns='urn:xmpp:mam:tmp' queryid='Q1' id='28482-98726-70990'>
				<forwarded xmlns='urn:xmpp:forward:0'>
					<delay xmlns='urn:xmpp:delay' stamp='2010-07-10T23:09:50Z'/>
					<message from='david@localhost/resource' to='roomid@conference.localhost' type='groupchat'>
						<body>
							<event>joined</event>
						</body>
					</message>
				</forwarded>
			</result>
		</message>
		..........
   
   After finishing sending all matching result, an <iq> result will be sent to inform the complete::

   		<iq type='result' id='A1' from='muclogchat.localhost' to='david@localhost/resource'>
	        <query xmlns='urn:xmpp:mam:tmp' queryid='Q1'>
	                <with>roomid@conference.localhost</with>
	                <start>2010-06-07T00:00:00Z</start>
	                <end>2010-07-07T05:03:27Z</end>
	                <set xmlns='http://jabber.org/protocol/rsm'>
	                        <first index='0'>28482-98726-73623</first>
	                        <last>28482-98726-80012</last>
	                        <count>1000</count>
	                </set>
	        </query>
		</iq> 
   

