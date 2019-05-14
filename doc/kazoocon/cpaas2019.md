# CPaaS in 2019

Let's talk integrations!

## Voice / Video Integrations

Integrate with other voice and video apps or add those capabilities to your app

### Javascript

Use [libWebPhone](https://github.com/2600hz/libwebphone) to integrate with WebRTC / Javascript clients.

## Managing Customers

### Dial out to endpoints

Use [Conference Dial](https://docs.2600hz.com/dev/applications/crossbar/doc/conference/#dialing-an-endpoint) to call out to DIDs, SIP URIs, Devices, or Users and put them into a conference bridge.

### Handle inbound

Using [Pivot](https://docs.2600hz.com/dev/applications/pivot/doc/requests/), control what to do with each caller based on your company's goals:

- Deny access to support if the caller has no support contract
- Check external (to KAZOO) data sources to inform routing decisions
- Build flexible menu flows in your programming language of choice

### Monitoring

By handling hangup causes (`RECOVERY_ON_TIMER_EXIRE` anyone?) and registration statuses, infer if a client site is experiencing issues and proactively reach out.

Check destinations dialed and correlate with fraud checks. Use the [Channels API](https://docs.2600hz.com/dev/applications/crossbar/doc/channels/) to shutdown the calls.

### Billing

Receive CDR data:
- Realtime via [websockets](https://docs.2600hz.com/dev/applications/crossbar/doc/websockets/)
- Almost realtime via [webhooks](https://docs.2600hz.com/dev/applications/webhooks/doc/events/channels/)
- Query the [CDRs API](https://docs.2600hz.com/dev/applications/crossbar/doc/cdrs/)
- Have KAZOO store them via [storage plans](https://docs.2600hz.com/dev/applications/crossbar/doc/storage/)
  - S3, Google Storage/Drive, Azure, Dropbox, FTP sites, HTTP URL, OneDrive

### Recordings

- Configure callflows to start recording
- Configure implicit ![call recording](https://docs.2600hz.com/dev/doc/user_guides/call_recording/) on devices, users, and accounts
  - Fine tune based on call direction and destination
- Store recordings using [storage plans](https://docs.2600hz.com/dev/applications/crossbar/doc/storage/)
  - S3, Google Storage/Drive, Azure, Dropbox, FTP sites, HTTP URL, OneDrive
  - Post-process to provide more value to your customers

### Compliance

- Use [Pivot](https://docs.2600hz.com/dev/applications/pivot/doc/requests/) to check CRM if patient has expressly allowed receiving text messages or phone calls before making an outbound call
  - Includes automated telephone calls
- Record all calls for regulatory compliance
  - Pair with 2600Hz Mobile to mitigate personal cell phone usage
  - Post-process to check audio quality: bad recording = no recording

### Live Chat

Use WebRTC to add a voice component to:

- Games
- Forums
- Customer Support widgets
- Streaming sites (like a Twitch.tv)
- Education
- Social Media
- Dating sites

In-browser, no-install voice calls that can incorporate PSTN callers as well if needed.
