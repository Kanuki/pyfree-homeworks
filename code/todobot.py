"""Todo Bot - A Telegram bot for managing todo lists."""

from random import choice

import telebot

# Configuration
TOKEN = ''

# Initialize bot
bot = telebot.TeleBot(TOKEN)

# Constants
RANDOM_TASKS = [
    'Написать Гвидо письмо',
    'Выучить Python',
    'Записаться на курс в Нетологию',
    'Посмотреть 4 сезон Рик и Морти'
]

HELP_TEXT = '''
Список доступных команд:
* /show <date> - показать все задачи на заданную дату
* /add <date> <task> - добавить задачу
* /random - добавить на сегодня случайную задачу
* /help - Напечатать справку
'''

# In-memory storage for todos {date: [tasks]}
todos = dict()


def add_todo(date, task):
    """Add a task to the todo list for the specified date."""
    date = date.lower()
    if date not in todos:
        todos[date] = []
    todos[date].append(task)


@bot.message_handler(commands=['help'])
def help_command(message):
    """Send help message with available commands."""
    bot.send_message(message.chat.id, HELP_TEXT)


@bot.message_handler(commands=['random'])
def random_command(message):
    """Add a random task for today."""
    task = choice(RANDOM_TASKS)
    add_todo('сегодня', task)
    bot.send_message(message.chat.id, f'Задача {task} добавлена на сегодня')


@bot.message_handler(commands=['add'])
def add_command(message):
    """Add a task to a specific date. Usage: /add <date> <task>"""
    parts = message.text.split(maxsplit=2)
    if len(parts) < 3:
        bot.send_message(message.chat.id, 'Использование: /add <дата> <задача>')
        return
    
    _, date, task = parts
    add_todo(date, task)
    bot.send_message(message.chat.id, f'Задача {task} добавлена на дату {date}')


@bot.message_handler(commands=['show'])
def show_command(message):
    """Show all tasks for a specific date. Usage: /show <date>"""
    parts = message.text.split()
    if len(parts) < 2:
        bot.send_message(message.chat.id, 'Использование: /show <дата>')
        return
    
    date = parts[1].lower()
    if date in todos:
        tasks_list = '\n'.join(f'[ ] {task}' for task in todos[date])
        bot.send_message(message.chat.id, tasks_list)
    else:
        bot.send_message(message.chat.id, 'Такой даты нет')


if __name__ == '__main__':
    bot.polling(none_stop=True)
