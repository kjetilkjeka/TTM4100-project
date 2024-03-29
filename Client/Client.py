# -*- coding: utf-8 -*-
import json
import socket
import os
from MessageReceiver import MessageReceiver
from MessageParser import MessageParser

class Client:
    """
    This is the chat client class
    """

    def __init__(self, host, server_port):
        """
        This method is run when creating a new Client object
        """
        # initialization
        self.server_port = server_port
        self.host = host

        self.screen_buffer = []
        
        # Set up the socket connection to the server
        self.connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        # Initiate the connection to the server
        self.connection.connect((self.host, self.server_port))

        # Start the message receiver
        self.message_receiver = MessageReceiver(self, self.connection)
        
        # TODO: Finish init process with necessary code
        self.run()

    def run(self):

        while True:
            command_text = raw_input('Enter command:\n')
            command_list = command_text.split(" ", 1)
            request = command_list[0]

            if(len(command_list) >= 2):
                content = command_list[1]
            else:
                content = "None"
            
            self.send_message(request, content)
            
    def disconnect(self):
        print "Program will exit"
        os._exit(1) # soemwhat bad hack
        # TODO: Handle disconnection
        pass

    def print_line(self, line):
        # TODO: Handle incoming message
        # This is realy a formating function
        self.screen_buffer.append(line)
        self.redraw_chat()
        
    def redraw_chat(self):
        for i in range(0,30):
            print('')
        for line in self.screen_buffer:
            print(line)
                
        print('Enter command: ')

    def send_message(self, request, content):
        payload = json.dumps({'request':request, 'content':content})
        self.connection.send(payload)
        pass
        
    # More methods may be needed!


if __name__ == '__main__':
    """
    This is the main method and is executed when you type "python Client.py"
    in your terminal.

    No alterations are necessary
    """
    client = Client('78.91.3.67', 9998)
