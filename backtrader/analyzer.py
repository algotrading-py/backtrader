#!/usr/bin/env python
# -*- coding: utf-8; py-indent-offset:4 -*-
###############################################################################
#
# Copyright (C) 2015-2023 Daniel Rodriguez
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
from __future__ import absolute_import, division, print_function, unicode_literals

import calendar
import datetime
import pprint as pp
from collections import OrderedDict

import backtrader as bt
from backtrader import TimeFrame
from backtrader.utils.py3 import MAXINT, with_metaclass


class MetaAnalyzer(bt.MetaParams):
    def donew(cls, *args, **kwargs):
        """
        Intercept the strategy parameter
        """
        # Create the object and set the params in place
        _obj, args, kwargs = super(MetaAnalyzer, cls).donew(*args, **kwargs)

        _obj._children = list()

        _obj.strategy = strategy = bt.metabase.findowner(_obj, bt.Strategy)
        _obj._parent = bt.metabase.findowner(_obj, Analyzer)

        # Register with a master observer if created inside one
        masterobs = bt.metabase.findowner(_obj, bt.Observer)
        if masterobs is not None:
            masterobs._register_analyzer(_obj)

        _obj.datas = strategy.datas

        # For each data add aliases: for first data: data and data0
        if _obj.datas:
            _obj.data = data = _obj.datas[0]

            for l, line in enumerate(data.lines):
                linealias = data._getlinealias(l)
                if linealias:
                    setattr(_obj, "data_%s" % linealias, line)
                setattr(_obj, "data_%d" % l, line)

            for d, data in enumerate(_obj.datas):
                setattr(_obj, "data%d" % d, data)

                for l, line in enumerate(data.lines):
                    linealias = data._getlinealias(l)
                    if linealias:
                        setattr(_obj, "data%d_%s" % (d, linealias), line)
                    setattr(_obj, "data%d_%d" % (d, l), line)

        _obj.create_analysis()

        # Return to the normal chain
        return _obj, args, kwargs

    def dopostinit(cls, _obj, *args, **kwargs):
        _obj, args, kwargs = super(MetaAnalyzer, cls).dopostinit(_obj, *args, **kwargs)

        if _obj._parent is not None:
            _obj._parent._register(_obj)

        # Return to the normal chain
        return _obj, args, kwargs


class Analyzer(with_metaclass(MetaAnalyzer, object)):
    """Базовый класс анализатора. Все анализаторы являются подклассами этого класса
    Экземпляр анализатора работает в рамках стратегии и обеспечивает анализ для этой стратегии.
    Автоматически установленные атрибуты:
      - ``self.strategy`` (предоставляет доступ к *стратегии* и всему,
        что доступно из нее)
      - ``self.datas[x]`` дает доступ к массиву источников данных, присутствующих в
        системе, к которым также можно получить доступ через ссылку на стратегию
      - ``self.data``, дает доступ к ``self.datas[0]``

      - ``self.dataX`` -> ``self.datas[X]``

      - ``self.dataX_Y`` -> ``self.datas[X].lines[Y]``

      - ``self.dataX_name`` -> ``self.datas[X].name``

      - ``self.data_name`` -> ``self.datas[0].name``

      - ``self.data_Y`` -> ``self.datas[0].lines[Y]``

    Это не объект *Lines*, но методы и операции следуют тому же
    дизайну
      - ``__init__`` во время создания экземпляра и начальной настройки
      - ``start`` / ``stop`` для сигнализации о начале и конце операций
      - семейство методов ``prenext`` / ``nextstart`` / ``next``, которые следуют
        вызовам, сделанным для тех же методов в стратегии
      - ``notify_trade`` / ``notify_order`` / ``notify_cashvalue`` /
        ``notify_fund``, которые получают те же уведомления, что и эквивалентные
        методы стратегии

    Режим работы открытый, и никакой конкретный шаблон не предпочтителен. Таким образом,
    анализ может быть сгенерирован с помощью вызовов ``next``, в конце операций
    во время ``stop`` и даже с помощью единственного метода, такого как ``notify_trade``

    Важно переопределить ``get_analysis``, чтобы вернуть объект типа *словарь*,
    содержащий результаты анализа (фактический формат зависит от реализации)

    """

    # ************************************************************************
    """Analyzer base class. All analyzers are subclass of this one

    An Analyzer instance operates in the frame of a strategy and provides an
    analysis for that strategy.

    Automagically set member attributes:

      - ``self.strategy`` (giving access to the *strategy* and anything
        accessible from it)

      - ``self.datas[x]`` giving access to the array of data feeds present in
        the the system, which could also be accessed via the strategy reference

      - ``self.data``, giving access to ``self.datas[0]``

      - ``self.dataX`` -> ``self.datas[X]``

      - ``self.dataX_Y`` -> ``self.datas[X].lines[Y]``

      - ``self.dataX_name`` -> ``self.datas[X].name``

      - ``self.data_name`` -> ``self.datas[0].name``

      - ``self.data_Y`` -> ``self.datas[0].lines[Y]``

    This is not a *Lines* object, but the methods and operation follow the same
    design

      - ``__init__`` during instantiation and initial setup

      - ``start`` / ``stop`` to signal the begin and end of operations

      - ``prenext`` / ``nextstart`` / ``next`` family of methods that follow
        the calls made to the same methods in the strategy

      - ``notify_trade`` / ``notify_order`` / ``notify_cashvalue`` /
        ``notify_fund`` which receive the same notifications as the equivalent
        methods of the strategy

    The mode of operation is open and no pattern is preferred. As such the
    analysis can be generated with the ``next`` calls, at the end of operations
    during ``stop`` and even with a single method like ``notify_trade``

    The important thing is to override ``get_analysis`` to return a *dict-like*
    object containing the results of the analysis (the actual format is
    implementation dependent)

    """
    csv = True

    def __len__(self):
        """Поддержка вызова ``len`` для анализаторов путем фактического возврата
        текущей длины стратегии, на которой работает анализатор
        Комментарий объясняет, что этот метод позволяет использовать функцию len() с анализаторами, при этом возвращается длина связанной стратегии.
        """
        """Support for invoking ``len`` on analyzers by actually returning the
        current length of the strategy the analyzer operates on"""
        return len(self.strategy)

    def _register(self, child):
        self._children.append(child)

    def _prenext(self):
        for child in self._children:
            child._prenext()

        self.prenext()

    def _notify_cashvalue(self, cash, value):
        for child in self._children:
            child._notify_cashvalue(cash, value)

        self.notify_cashvalue(cash, value)

    def _notify_fund(self, cash, value, fundvalue, shares):
        for child in self._children:
            child._notify_fund(cash, value, fundvalue, shares)

        self.notify_fund(cash, value, fundvalue, shares)

    def _notify_trade(self, trade):
        for child in self._children:
            child._notify_trade(trade)

        self.notify_trade(trade)

    def _notify_order(self, order):
        for child in self._children:
            child._notify_order(order)

        self.notify_order(order)

    def _nextstart(self):
        for child in self._children:
            child._nextstart()

        self.nextstart()

    def _next(self):
        for child in self._children:
            child._next()

        self.next()

    def _start(self):
        for child in self._children:
            child._start()

        self.start()

    def _stop(self):
        for child in self._children:
            child._stop()

        self.stop()

    #  все методы по умолчанию являются заглушками (pass), кроме prenext(), nextstart() и get_analysis(), которые имеют базовую реализацию.
    def notify_cashvalue(self, cash, value):
        """Получает уведомление о денежных средствах/стоимости перед каждым следующим циклом"""
        """Receives the cash/value notification before each next cycle"""
        pass

    def notify_fund(self, cash, value, fundvalue, shares):
        """Получает текущие денежные средства, стоимость, стоимость фонда и акции фонда""" """
        Receives the current cash, value, fundvalue and fund shares"""
        pass

    def notify_order(self, order):
        """Получает уведомления о заявках перед каждым следующим циклом""" """
        Receives order notifications before each next cycle"""
        pass

    def notify_trade(self, trade):
        """Получает уведомления о сделках перед каждым следующим циклом""" """
        Receives trade notifications before each next cycle"""
        pass

    def next(self):
        """Вызывается при каждом вызове next стратегии, после того как
        достигнут минимальный период стратегии"""
        """Invoked for each next invocation of the strategy, once the minum
        preiod of the strategy has been reached"""
        pass

    def prenext(self):
        """Вызывается при каждом вызове prenext стратегии, пока не будет
        достигнут минимальный период стратегии
        Поведение по умолчанию для анализатора - вызвать метод ``next``
        """
        """Invoked for each prenext invocation of the strategy, until the minimum
        period of the strategy has been reached
        The default behavior for an analyzer is to invoke ``next``
        """
        self.next()

    def nextstart(self):
        """Вызывается ровно один раз при вызове nextstart стратегии,
        когда минимальный период был впервые достигнут
        """
        """Invoked exactly once for the nextstart invocation of the strategy,
        when the minimum period has been first reached
        """
        self.next()

    def start(self):
        """Вызывается для обозначения начала операций, давая анализатору
        время для настройки необходимых вещей"""
        """Invoked to indicate the start of operations, giving the analyzer
        time to setup up needed things"""
        pass

    def stop(self):
        """Вызывается для обозначения окончания операций, давая анализатору
        время для завершения необходимых вещей"""
        """Invoked to indicate the end of operations, giving the analyzer
        time to shut down needed things"""
        pass

    def create_analysis(self):
        """Предназначен для переопределения подклассами. Дает возможность создать
        структуры, которые содержат анализ.
        Поведение по умолчанию - создать ``OrderedDict`` с именем ``rets``
        """
        """Meant to be overriden by subclasses. Gives a chance to create the
        structures that hold the analysis.
        The default behaviour is to create a ``OrderedDict`` named ``rets``
        """
        self.rets = OrderedDict()

    def get_analysis(self):
        """Возвращает объект типа *словарь* с результатами анализа
        Ключи и формат результатов анализа в словаре зависят
        от реализации.
        Даже не обязательно, чтобы результат был объектом типа *словарь*, это просто
        соглашение
        Реализация по умолчанию возвращает стандартный OrderedDict ``rets``,
        созданный методом ``create_analysis`` по умолчанию
        """
        """Returns a *dict-like* object with the results of the analysis

        The keys and format of analysis results in the dictionary is
        implementation dependent.

        It is not even enforced that the result is a *dict-like object*, just
        the convention

        The default implementation returns the default OrderedDict ``rets``
        created by the default ``create_analysis`` method

        """
        return self.rets

    def print(self, *args, **kwargs):
        """Печатает результаты, возвращаемые методом ``get_analysis``, через стандартный
        объект ``Writerfile``, который по умолчанию выводит данные в стандартный
        поток вывода
        """
        """Prints the results returned by ``get_analysis`` via a standard
        ``Writerfile`` object, which defaults to writing things to standard
        output
        """
        writer = bt.WriterFile(*args, **kwargs)
        writer.start()
        pdct = dict()
        pdct[self.__class__.__name__] = self.get_analysis()
        writer.writedict(pdct)
        writer.stop()

    def pprint(self, *args, **kwargs):
        """Печатает результаты, возвращаемые методом ``get_analysis``, используя модуль
        форматированной печати Python (*pprint*)
        """
        """Prints the results returned by ``get_analysis`` using the pretty
        print Python module (*pprint*)
        """
        pp.pprint(self.get_analysis(), *args, **kwargs)


class MetaTimeFrameAnalyzerBase(Analyzer.__class__):
    def __new__(meta, name, bases, dct):
        # Хак для поддержки оригинального названия метода
        # Hack to support original method name
        if "_on_dt_over" in dct:
            dct["on_dt_over"] = dct.pop("_on_dt_over")  # rename method

        return super(MetaTimeFrameAnalyzerBase, meta).__new__(meta, name, bases, dct)


class TimeFrameAnalyzerBase(with_metaclass(MetaTimeFrameAnalyzerBase, Analyzer)):
    params = (
        ("timeframe", None),
        ("compression", None),
        ("_doprenext", True),
    )

    def _start(self):
        # Переопределить для добавления специфичных атрибутов
        # Override to add specific attributes
        self.timeframe = self.p.timeframe or self.data._timeframe
        self.compression = self.p.compression or self.data._compression

        self.dtcmp, self.dtkey = self._get_dt_cmpkey(datetime.datetime.min)
        super(TimeFrameAnalyzerBase, self)._start()

    def _prenext(self):
        for child in self._children:
            child._prenext()

        if self._dt_over():
            self.on_dt_over()

        if self.p._doprenext:
            self.prenext()

    def _nextstart(self):
        for child in self._children:
            child._nextstart()

        if self._dt_over() or not self.p._doprenext:  # exec if no prenext
            self.on_dt_over()

        self.nextstart()

    def _next(self):
        for child in self._children:
            child._next()

        if self._dt_over():
            self.on_dt_over()

        self.next()

    def on_dt_over(self):
        pass

    def _dt_over(self):
        if self.timeframe == TimeFrame.NoTimeFrame:
            dtcmp, dtkey = MAXINT, datetime.datetime.max
        else:
            # Начиная с версии >= 1.9.x системная дата/время находится в стратегии
            # With >= 1.9.x the system datetime is in the strategy
            dt = self.strategy.datetime.datetime()
            dtcmp, dtkey = self._get_dt_cmpkey(dt)

        if self.dtcmp is None or dtcmp > self.dtcmp:
            self.dtkey, self.dtkey1 = dtkey, self.dtkey
            self.dtcmp, self.dtcmp1 = dtcmp, self.dtcmp
            return True

        return False

    def _get_dt_cmpkey(self, dt):
        if self.timeframe == TimeFrame.NoTimeFrame:
            return None, None

        if self.timeframe == TimeFrame.Years:
            dtcmp = dt.year
            dtkey = datetime.date(dt.year, 12, 31)

        elif self.timeframe == TimeFrame.Months:
            dtcmp = dt.year * 100 + dt.month
            _, lastday = calendar.monthrange(dt.year, dt.month)
            dtkey = datetime.datetime(dt.year, dt.month, lastday)

        elif self.timeframe == TimeFrame.Weeks:
            isoyear, isoweek, isoweekday = dt.isocalendar()
            dtcmp = isoyear * 100 + isoweek
            sunday = dt + datetime.timedelta(days=7 - isoweekday)
            dtkey = datetime.datetime(sunday.year, sunday.month, sunday.day)

        elif self.timeframe == TimeFrame.Days:
            dtcmp = dt.year * 10000 + dt.month * 100 + dt.day
            dtkey = datetime.datetime(dt.year, dt.month, dt.day)

        else:
            dtcmp, dtkey = self._get_subday_cmpkey(dt)

        return dtcmp, dtkey

    def _get_subday_cmpkey(self, dt):
        # Расчет внутридневной позиции
        # Calculate intraday position
        point = dt.hour * 60 + dt.minute

        if self.timeframe < TimeFrame.Minutes:
            point = point * 60 + dt.second

        if self.timeframe < TimeFrame.Seconds:
            point = point * 1e6 + dt.microsecond

        # Применяем сжатие для обновления позиции точки (например, при сжатии 5 -> 200 // 5)
        # # Apply compression to update point position (comp 5 -> 200 // 5)
        point = point // self.compression

        # Переходим к следующей границе
        # # Move to next boundary
        point += 1

        # Восстанавливаем точку во временные единицы путем обратного применения сжатия
        # # Restore point to the timeframe units by de-applying compression
        point *= self.compression

        # Получаем часы, минуты, секунды и микросекунды
        # # Get hours, minutes, seconds and microseconds
        if self.timeframe == TimeFrame.Minutes:
            ph, pm = divmod(point, 60)
            ps = 0
            pus = 0
        elif self.timeframe == TimeFrame.Seconds:
            ph, pm = divmod(point, 60 * 60)
            pm, ps = divmod(pm, 60)
            pus = 0
        elif self.timeframe == TimeFrame.MicroSeconds:
            ph, pm = divmod(point, 60 * 60 * 1e6)
            pm, psec = divmod(pm, 60 * 1e6)
            ps, pus = divmod(psec, 1e6)

        extradays = 0
        if ph > 23:  # перешли за полночь: # went over midnight:
            extradays = ph // 24
            ph %= 24

        # moving 1 minor unit to the left to be in the boundary
        # сдвигаем на 1 младшую единицу влево, чтобы быть на границе
        # pm -= self.timeframe == TimeFrame.Minutes
        # ps -= self.timeframe == TimeFrame.Seconds
        # pus -= self.timeframe == TimeFrame.MicroSeconds

        tadjust = datetime.timedelta(
            minutes=self.timeframe == TimeFrame.Minutes,
            seconds=self.timeframe == TimeFrame.Seconds,
            microseconds=self.timeframe == TimeFrame.MicroSeconds,
        )

        # Add extra day if present
        # Добавляем дополнительный день, если он есть
        if extradays:
            dt += datetime.timedelta(days=extradays)

        # Replace intraday parts with the calculated ones and update it
        # Заменяем внутридневные части рассчитанными значениями и обновляем их
        dtcmp = dt.replace(hour=ph, minute=pm, second=ps, microsecond=pus)
        dtcmp -= tadjust
        dtkey = dtcmp

        return dtcmp, dtkey
