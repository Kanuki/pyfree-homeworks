"""Echo Bot - A simple Telegram bot that echoes messages back."""

import telebot

# Configuration
TOKEN = ''

# Initialize bot
bot = telebot.TeleBot(TOKEN)


@bot.message_handler(content_types=["text"])
def echo_message(message):
    """Echo received text message back to the sender."""
    bot.send_message(message.chat.id, message.text)


if __name__ == '__main__':
    bot.polling(none_stop=True)
