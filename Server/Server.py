# -*- coding: utf-8 -*-
import SocketServer
import json
from threading import Thread

"""
Variables and functions that must be used by all the ClientHandler objects
must be written here (e.g. a dictionary for connected clients)
"""

message_distributor = None
 
class MessageDistributor(Thread):
    def __init__(self):
        self.subscriber_list = []
        # add message history as well
        
    def subscribe(self, ClientHandler):
        self.subscriber_list.append(ClientHandler)

    def new_message(self, Message):
        for client in self.subscriber_list:
            client.send(Message)
    

class ClientHandler(SocketServer.BaseRequestHandler):
    """
    This is the ClientHandler class. Everytime a new client connects to the
    server, a new ClientHandler object will be created. This class represents
    only connected clients, and not the server itself. If you want to write
    logic for the server, you must write it outside this class
    """

    def handle(self):
        """
        This method handles the connection between a client and the server.
        """
        self.ip = self.client_address[0]
        self.port = self.client_address[1]
        self.connection = self.request
        
        print("a new handler was spawned") # temp

        message_distributor.subscribe(self)
        
        # Loop that listens for messages from the client

        # handle login

        while True:
            received_string = self.connection.recv(4096)

            print("received_string") #temp
            
            # parse

            # do checks
            # if illegal characters is used

            command = 'message' # temp
            
            if command == 'message':
                # new message from user
                # broadcast to all clients
                message_distributor.new_message(json.dumps({'timestamp':1, 'sender':'me', 'response':'someresponse', 'content':'somecontent'})) # temp loopback
            elif command == 'error':
                # send message to client
                pass
            elif command == 'info':
                # send informational response to server
                pass
            elif command == 'history':
                # list all messages that is issued
                pass
                                    
                                     
            # TODO: Add handling of received payload from client
    def send(self, message):
        self.connection.send(message)
            

class ThreadedTCPServer(SocketServer.ThreadingMixIn, SocketServer.TCPServer):
    """
    This class is present so that each client connected will be ran as a own
    thread. In that way, all clients will be served by the server.

    No alterations are necessary
    """
    allow_reuse_address = True

if __name__ == "__main__":
    """
    This is the main method and is executed when you type "python Server.py"
    in your terminal.

    No alterations are necessary
    """
    HOST, PORT = 'localhost', 9998
    print 'Server running...'

    message_distributor = MessageDistributor()
    # Set up and initiate the TCP server
    server = ThreadedTCPServer((HOST, PORT), ClientHandler)
    server.serve_forever()
