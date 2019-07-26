const remotedev = require('remotedev');

const ws = remotedev.connectViaExtension({ port: 8000 });

ws.subscribe((v) => {
  try {
    console.log(v);
    const json = typeof v.payload === "string" ? JSON.parse(v.payload) : v.payload;
    ws.send(json.action, json.payload);
  } catch (error) {
    console.log(error);
  }
});

ws.init();
// ws.send('TEST', {});
// ws.error({ error: true });
