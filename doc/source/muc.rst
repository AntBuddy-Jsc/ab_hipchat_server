MUC
===

Our performance of muc room with the following rules:

- :ref:`Create Room<section1>`
- :ref:`Invite User<section2>`
- :ref:`Change Subject<section3>`
- :ref:`Romove Occupant<section4>`
- :ref:`Destroy Room<section5>`
- :ref:`Change Avaliable Status<section6>`
- :ref:`Change Room Configuration<section7>` 

On our system, only owner (admin) is allowed to invite, remove the member list and change subject. Normal member does not allow to do this. 
All rules introduce in this document follow the `XEP-0045`_ - Multi-User Chat.

.. _XEP-0045 : http://xmpp.org/extensions/xep-0045.html

To make a rule for room chat, we will use 3 user such as tester1, tester2, tester3.

.. _section1:

1. Create Room
 
  The first, tester1 will send a <presence/> to create a room if room no exist::

	<presence from='tester1@localhost/telnet to='room1@conference.localhost/nickname1'>
		<x xmlns='http://jabber.org/protocol/muc'/>
	</presence>
  
  Server response with <presence/>::

  	<presence from='room1@conference.localhost/nickname1' to='tester1@localhost/telnet' xml:lang=''>
  		<x xmlns='http://jabber.org/protocol/muc#user'>
  			<item jid='tester1@localhost/telnet' affiliation='owner' role='moderator'/>
			<status code='110'/>
			<status code='201'/>
  		</x>
  	</presence>

  	<message from='room1@conference.localhost' to='tester1@localhost/telnet' type='groupchat'>
  		<subject/>
  		<body/>
  	</message>

  .. Note ::

	   - Code 201 : to acknowledge that the room no exist and are created. Without this code, that means that room existed.
	   - tester1 is room's owner and is added to room.
	   - After sending an <presence/> response, server continue send an <message> with an <subject> element is empty.
  
  If it is new room, the first time it's clocked. Room owner must send <iq/> to unlock and tell to server that all default configure will be applied to this room::

	<iq id='C1' from='tester1@localhost/telnet to='room1@conference.localhost' type='set'>
		<query xmlns='http://jabber.org/protocol/muc#owner'>
			<x xmlns='jabber:x:data' type='submit'/>
		</query>
	</iq>
  
  Server response to inform a successful room creation ::

  	<iq from='room1@conference.localhost' to='tester1@localhost/telnet' id='C1' type='result'>
  		<query xmlns='http://jabber.org/protocol/muc#owner'/>
  	</iq>

.. _section2:

2. Invite To Room
	
	* Room Member

		* Owner Invite

		  Owner invites user tester2 to room ::

		  		<message id='INV1' from='tester1@localhost/telnet' to='room1@conference.localhost'>
		  			<x xmlns='http://jabber.org/protocol/muc#user'>
		  				<invite to='tester2@localhost'>
		  					<reason/>
		  				</invite>
		  			</x>
		  		</message>

		  Server sends to tester2 the following message::

		  	<message from='room1@conference.localhost' to='tester2@localhost' type='normal'>
		  		<x xmlns='http://jabber.org/protocol/muc#user'>
		  			<invite from='tester1@localhost/telnet'>
		  				<reason></reason>
		  			</invite>
		  		</x>
		  		<x xmlns='jabber:x:conference' jid='room1@conference.localhost'>
		  		</x>
		  		<body>tester1@localhost/telnet invites you to the room room1@conference.localhost</body>
		  	</message>

		  After this, tester2 now is member of room1.

		  tester2 can decline the invitation by send a following message::

		  	<message id='Decl1' to='room1@conference.localhost'>
		  		<x xmlns='http://jabber.org/protocol/muc#user'>
		  			<decline to='tester1@localhost'>
		  				<reason></reason>
		  			</decline>
		  		</x>
		  	</message>

		  Room inform to owner that invitation is declined::

		  	<message from='room1@conference.localhost' to='tester1@localhost' xml:lang='' id='Decl1'>
		  		<x xmlns='http://jabber.org/protocol/muc#user'>
		  			<decline from='tester4@localhost/telnet'>
		  				<reason/>
		  			</decline>
		  		</x>
		  	</message>

		* Member Invite

		  Member (not owner) invites user to room::

		  	<message id='INV2' to='room1@conference.localhost'>
		  		<x xmlns='http://jabber.org/protocol/muc#user'>
		  			<invite to='tester3@localhost'>
		  				<reason></reason>
		  			</invite>
		  		</x>
		  	</message>

		  Server responses an <forbidden/> message::

		  	<message from='room1@conference.localhost' to='tester2@localhost/telnet' type='error' xml:lang='' id='IV2'>
		  		<x xmlns='http://jabber.org/protocol/muc#user'>
		  			<invite to='tester3@localhost'>
		  				<reason/>
		  			</invite>
		  		</x>
		  		<error code='403' type='auth'>
		  			<forbidden xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
		  		</error>
		  	</message>

	* Enter The Room

		* Member Enter The Room

		  After an invitation, a member can send an request to enter to chat room::

				<presence id='J1' from='tester2@localhost/telnet' to='room1@conference.localhost/nickname2'>
					<x xmlns='http://jabber.org/protocol/muc'/>
				</presence>

		  Success enter the room, tester2 receives all presence of existed occupant in room include itself::

				<presence from='room1@conference.localhost/nickname1' to='tester2@localhost/telnet' xml:lang=''>
					<x xmlns='http://jabber.org/protocol/muc#user'>
						<item affiliation='owner' role='moderator'/>
					</x>
				</presence>

				<presence from='room1@conference.localhost/nickname2' to='tester2@localhost/telnet' xml:lang='' id='R1'>
					<x xmlns='http://jabber.org/protocol/muc#user'>
						<item affiliation='member' role='participant'/>
						<status code='110'/>
					</x>
				</presence>

				<message from='room1@conference.localhost' to='tester2@localhost/telnet' type='groupchat'>
					<subject/>
					<body/>
				</message>

		  .. Note :: 

			   - Code 110 : is attached to an self-presence so that member knows that it is prefered as an occupant.
			   - Self-presence to make tester2 knows that it has finished receiving the room roster.
			   - After finishing broadcast all presences to new occupant, server continue send an <message/> with current subject. If no subject is set, return an empty <subject/>.

		  The same time, all existed occupant also receives presence of new occupant::

				<presence from='room1@conference.localhost/nickname2' to='tester1@localhost/telnet' xml:lang=''>
					<x xmlns='http://jabber.org/protocol/muc#user'>
						<item jid='tester2@localhost/telnet' affiliation='member' role='participant'/>
					</x>
				</presence>

		* Not Member Enter The Room

			An normal user not member of room tries to enter the room::

				<presence  id='J2' from='tester3@localhost/telnet' to='room1@conference.localhost/nickname3'>
			  		<x xmlns='http://jabber.org/protocol/muc'/>
			  	</presence> 

			Server will response an error message::

				<presence from='room1@conference.localhost/nickname3' to='tester3@localhost/telnet' type='error' xml:lang=''>
					<x xmlns='http://jabber.org/protocol/muc'/>
						<error code='407' type='auth'>
							<registration-required xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
								<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
									Membership is required to enter this room
								</text>
						</error>
				</presence>

.. _section3:

3. Change The Subject
	
	* Owner Request

		Owner requests to change room subject::

			<message id='Sub1' from='tester1@localhost/telnet' to='room1@conference.localhost' type='groupchat'>
				<subject>Fire Burn and Cauldron Bubble!</subject>
			</message>

		Server braodcasts to all occupants about subject change::

			<message from='room1@conference.localhost/nickname1' to='tester2@localhost/telnet' xml:lang='' id='Sub2' type='groupchat'>
				<subject>Fire Burn and Cauldron Bubble!</subject>
			</message>

	* Member Request

		Member requests to change the room subject::

			<message id='Sub2' from='tester2@localhost/telnet' to='room1@conference.localhost' type='groupchat'>
				<subject>This is music room!</subject>
			</message>

		Server responses an <forbiddent> message::

			<message from='room1@conference.localhost/nickname2' to='tester2@localhost/telnet' type='error' xml:lang='' id='Sub3'>
				<subject>This is music room!</subject>
				<error code='403' type='auth'>
					<forbidden xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
					<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
						Only moderators are allowed to change the subject in this room
					</text>
				</error>
			</message>

.. _section4:

4. Remove Occupant
	
	* Owner Request.

		Owner removes member tester2 from room::

			<iq id='RV1' to='room1@conference.localhost'type='set'>
				<query xmlns='http://jabber.org/protocol/muc#admin'>
					<item affiliation='none' jid='tester2@localhost'/>
				</query>
			</iq>

		Server responses to owner to inform an success::

			<iq from='room1@conference.localhost' to='tester1@localhost/telnet' id='RV1' type='result'>
				<query xmlns='http://jabber.org/protocol/muc#admin'/>
			</iq>

		And server also broadcasts to all occupants include the removed occupant to declare an removal::

			<presence from='room1@conference.localhost/nickname2' to='tester2@localhost/telnet' type='unavailable'>
				<x xmlns='http://jabber.org/protocol/muc#user'>
					<item affiliation='none' role='none'/>
					<status code='321'/>
				</x>
			</presence>

			<presence from='room1@conference.localhost/nickname2' to='tester3@localhost/telnet' type='unavailable'>
				<x xmlns='http://jabber.org/protocol/muc#user'>
					<item affiliation='none' role='none'/>
					<status code='321'/>
				</x>
			</presence>

		After all, tester2 is not member of room more.

		.. Note::

			To add an removed occupant to room again, the client just resend an invite message, following an request to enter the room.

	* Member Request

		Assume that tester2 and tester3 is member of room and not owner. tester2 removes tester3 from room::

			<iq id='RV2' to='room1@conference.localhost'type='set'>
				<query xmlns='http://jabber.org/protocol/muc#admin'>
					<item affiliation='none' jid='tester3@localhost'/>
				</query>
			</iq>
 
		Server responses an <forbiddent> message::

			<iq from='room1@conference.localhost' to='tester2@localhost/telnet' id='RV2' type='error'>
				<query xmlns='http://jabber.org/protocol/muc#admin'>
					<item affiliation='none' jid='tester3@localhost'/>
				</query>
				<error code='403' type='auth'>
					<forbidden xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
				</error>
			</iq>

.. _section5:

5. Destroy Room

	* Owner Request

		Owner requests to destroy room::

			<iq id='Det2' to='room1@conference.localhost' type='set'>
				<query xmlns='http://jabber.org/protocol/muc#owner'>
					<destroy jid='room1@conference.localhost'>
						<reason></reason
					</destroy>
				</query>
			</iq>

		Server broadcasts presence type='unavailable' to all occupants to inform that occupant is removed from room::

			<presence from='room1@conference.localhost/nickname1' to='tester1@localhost/telnet' type='unavailable'>
				<x xmlns='http://jabber.org/protocol/muc#user'>
					<item affiliation='none' role='none'/>
					<destroy jid='room1@conference.localhost'>
						<reason/>
					</destroy>
				</x>
			</presence>

			<presence from='room1@conference.localhost/nickname2' to='tester2@localhost/telnet' type='unavailable'>
				<x xmlns='http://jabber.org/protocol/muc#user'>
					<item affiliation='none' role='none'/>
					<destroy jid='room1@conference.localhost'>
						<reason/>
					</destroy>
				</x>
			</presence>

		And server also informs owner of successful Destruction::

			<iq from='room1@conference.localhost' to='tester1@localhost/telnet' id='Det2' type='result'>
				<query xmlns='http://jabber.org/protocol/muc#owner'/>
			</iq>


	* Member Request

		Member tester2 not owner request destroy room::

			<iq id='Det1' to='room1@conference.localhost' type='set'>
				<query xmlns='http://jabber.org/protocol/muc#owner'>
					<destroy jid='room1@conference.localhost'>
						<reason></reason>
					</destroy>
				</query>
			</iq>

		Server responses an <forbidden/> message::

			<iq from='room1@conference.localhost' to='tester2@localhost/telnet' id='Det1' type='error'>
				<query xmlns='http://jabber.org/protocol/muc#owner'>
					<destroy jid='room1@conference.localhost'>
						<reason/>
					</destroy>
				</query>
				<error code='403' type='auth'>
					<forbidden xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'/>
					<text xmlns='urn:ietf:params:xml:ns:xmpp-stanzas'>
						Owner privileges required
					</text>
				</error>
			</iq>

.. _section6:

6. Change Avaliable Status
	
	Occupant tester1 change available status::

		<presence from='tester1@localhost/telnet' id='St1' to='romm1@conference.localhost'>
			<show>away</show>
			<status>
				gone where the goblins go
			</status>
		</presence>

	Server broadcasts to all occupants include itself::

		<presence from='romm1@conference.localhost/nickname1' to='tester1@localhost/telnet' xml:lang='' id='St1'>
			<show>away</show>
			<status>gone where the goblins go</status>
			<x xmlns='http://jabber.org/protocol/muc#user'>
				<item jid='tester1@localhost/telnet' affiliation='owner' role='moderator'/>
				<status code='110'/>
			</x>
		</presence>

		<presence from='romm1@conference.localhost/nickname1' to='tester2@localhost/telnet' xml:lang='' id='St1'>
			<show>away</show>
			<status>gone where the goblins go</status>
			<x xmlns='http://jabber.org/protocol/muc#user'>
				<item affiliation='owner' role='moderator'/>
			</x>
		</presence>

.. _section7:

7. Change Room Configuration
	
   .. Note::

      The configuration form have more fields than the one below. However with currently version, just apply below form as an standard to all existed rooms which created with boris code . 

   Owner send <iq/> contain an desire configuration form as follow::

  		<iq  id='Config'  to='room@conference.localhost' type='set'>
  			<query xmlns='http://jabber.org/protocol/muc#owner'>
  				<x xmlns='jabber:x:data' type='submit'>
  					<field var='FORM_TYPE'>
  						<value>http://jabber.org/protocol/muc#roomconfig</value>
  					</field>
  					<field type='boolean' label='Make room persistent' var='muc#roomconfig_persistentroom'>
  						<value>1</value>
  					</field>
  					<field type='boolean' label='Make room members-only' var='muc#roomconfig_membersonly'>
  						<value>1</value>
  					</field>
  					<field type='boolean' label='Allow users to change the subject' var='muc#roomconfig_changesubject'>
  						<value>0</value>
  					</field>
  					<field type='boolean' label='Allow users to send private messages' var='allow_private_messages'>
  						<value>0</value>
  					</field>
  				</x>
  			</query>
  		</iq>

  Update configuration successfully, server response to Owner ::

  		<iq from='room@conference.localhost' to='tester1@localhost/telnet' id='Config' type='result'>
  			<query xmlns='http://jabber.org/protocol/muc#owner'/>
  		</iq>





  


