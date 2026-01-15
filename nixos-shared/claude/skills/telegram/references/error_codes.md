# Telegram API Error Reference

Common error codes and solutions when using Telegram Bot API.

## Authentication Errors

**401 Unauthorized**
- Invalid or expired bot token
- Solution: Verify TELEGRAM_BOT_TOKEN is correct
- Get token from @BotFather in Telegram

## Bad Request Errors

**400 Bad Request**
- Invalid parameters in API call
- Common causes:
  - Invalid chat_id format
  - Message text too long (>4096 chars)
  - Invalid file format or size
  - Missing required parameters
- Solution: Check parameter format and values

## Permission Errors

**403 Forbidden**
- Bot cannot message the chat
- Causes:
  - User hasn't started conversation with bot (/start command)
  - User has blocked the bot
  - Bot not added to group/channel
  - Bot lacks permissions in group
- Solution: User must initiate contact first or add bot to chat

## Rate Limiting

**429 Too Many Requests**
- API rate limit exceeded
- Response includes `retry_after` parameter (seconds to wait)
- Limits:
  - ~30 messages/second to different chats
  - ~1 message/second to same chat
- Solution: Implement exponential backoff, reduce request rate

## Not Found Errors

**404 Not Found**
- Resource doesn't exist
- Causes:
  - Invalid chat_id
  - Message/file was deleted
  - Bot removed from chat
- Solution: Verify chat_id exists and bot has access

## File Errors

**Request Entity Too Large**
- File exceeds size limits
- Limits:
  - Photos: 10MB
  - Documents: 50MB
  - Videos: 50MB
- Solution: Compress or split file

## Network Errors

**Connection timeout**
- Network connectivity issue
- Telegram servers unreachable
- Solution: Retry with exponential backoff

## Error Handling Best Practices

1. **Always check error codes** - Don't just catch generic exceptions
2. **Respect rate limits** - Implement retry logic with delays
3. **Validate inputs** - Check chat_id, file sizes before API calls
4. **Handle 403 gracefully** - Inform user they need to start bot first
5. **Log errors** - Keep track of error patterns for debugging
