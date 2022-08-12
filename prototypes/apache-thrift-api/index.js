"use strict";
exports.__esModule = true;
var thrift_1 = require("thrift");
var Network_1 = require("./src/ts/Network");
require("./src/ts/wallet_types");
var conn = (0, thrift_1.createConnection)("localhost", 8888, {
    transport: thrift_1.TFramedTransport,
    protocol: thrift_1.TBinaryProtocol
});
var client = (0, thrift_1.createClient)(Network_1.Client, conn);
client.latestBlockHeader(function (error, blockHeader) {
    console.log(error);
    console.log(blockHeader);
    conn.end();
});
