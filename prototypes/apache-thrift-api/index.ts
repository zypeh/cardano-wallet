import { createConnection
       , TFramedTransport
       , TBinaryProtocol
       , createClient
       , Connection} from "thrift";
import { Client } from "./src/ts/Network";
import "./src/ts/wallet_types";

const conn:Connection = createConnection("localhost", 8888, {
    transport : TFramedTransport,
    protocol : TBinaryProtocol
  });

const client:Client = createClient(Client, conn);
client.latestBlockHeader((error, blockHeader) => {
    console.log(error);
    console.log(blockHeader);
    conn.end();
});
