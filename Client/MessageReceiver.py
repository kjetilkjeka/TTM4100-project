# -*- coding: utf-8 -*-
from threading import Thread
from MessageParser import MessageParser

class MessageReceiver(Thread):
    """
    This is the message receiver class. The class inherits Thread, something that
    is necessary to make the MessageReceiver start a new thread, and it allows
    the chat client to both send and receive messages at the same time
    """

    def __init__(self, client, connection):
        """
        This method is executed when creating a new MessageReceiver object
        """
        
        # Flag to run thread as a deamon
        Thread.__init__(self)
        self.daemon = True

        # TODO: Finish initialization of MessageReceiver
        self.client = client
        self.connection = connection

        self.start()
        
        
    def run(self):
        # TODO: Make MessageReceiver receive and handle payloads
        while True:
            message_raw = self.connection.recv(4096) # bit to hardcoded?
            if not message_raw:
                self.client.disconnect()
            print("MessageReceiver received command") # temp
            message_parser = MessageParser()
            response = message_parser.parse(message_raw)
            self.client.receive_message(response)
        
            
