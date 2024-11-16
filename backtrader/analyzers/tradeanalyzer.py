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

import sys

from backtrader import Analyzer
from backtrader.utils import AutoDict, AutoOrderedDict
from backtrader.utils.py3 import MAXINT


class TradeAnalyzer(Analyzer):
    """
        Предоставляет статистику по закрытым сделкам (также ведет учет открытых)
        - Всего Открытых/Закрытых сделок
        - Серия Выигрышей/Проигрышей Текущая/Самая длинная
        - Прибыль и Убытки Всего/В среднем
        - Выигрышные/Проигрышные Количество/Общий P&L/Средний P&L/Максимальный P&L
        - Длинные/Короткие Количество/Общий P&L/Средний P&L/Максимальный P&L
            - Выигрышные/Проигрышные Количество/Общий P&L/Средний P&L/Максимальный P&L
        - Длительность (баров в рынке)
        - Всего/Среднее/Максимум/Минимум
        - Выигрышные/Проигрышные Всего/Среднее/Максимум/Минимум
        - Длинные/Короткие Всего/Среднее/Максимум/Минимум
            - Выигрышные/Проигрышные Всего/Среднее/Максимум/Минимум
    Примечание:
        Анализатор использует "авто"-словарь для полей, что означает: если не было
        совершено сделок, статистика не будет сгенерирована.
        В этом случае в словаре, возвращаемом методом ``get_analysis``, будет
        только одно поле/подполе, а именно:
        - dictname['total']['total'], которое будет иметь значение 0 (к этому полю
            также можно обратиться через точечную нотацию dictname.total.total)
            ----------------------------------------------------------

            Provides statistics on closed trades (keeps also the count of open ones)

              - Total Open/Closed Trades

              - Streak Won/Lost Current/Longest

              - ProfitAndLoss Total/Average

              - Won/Lost Count/ Total PNL/ Average PNL / Max PNL

              - Long/Short Count/ Total PNL / Average PNL / Max PNL

                  - Won/Lost Count/ Total PNL/ Average PNL / Max PNL

              - Length (bars in the market)

                - Total/Average/Max/Min

                - Won/Lost Total/Average/Max/Min

                - Long/Short Total/Average/Max/Min

                  - Won/Lost Total/Average/Max/Min

            Note:

              The analyzer uses an "auto"dict for the fields, which means that if no
              trades are executed, no statistics will be generated.

              In that case there will be a single field/subfield in the dictionary
              returned by ``get_analysis``, namely:

                - dictname['total']['total'] which will have a value of 0 (the field is
                  also reachable with dot notation dictname.total.total
    """

    def create_analysis(self):
        self.rets = AutoOrderedDict()
        self.rets.total.total = 0

    def stop(self):
        super(TradeAnalyzer, self).stop()
        self.rets._close()

    def notify_trade(self, trade):
        if trade.justopened:
            # Trade just opened Сделка только что открыта
            self.rets.total.total += 1
            self.rets.total.open += 1

        elif trade.status == trade.Closed:
            trades = self.rets

            res = AutoDict()
            # Trade just closed Сделка только что закрыта

            won = res.won = int(trade.pnlcomm >= 0.0)
            lost = res.lost = int(not won)
            tlong = res.tlong = trade.long
            tshort = res.tshort = not trade.long

            trades.total.open -= 1
            trades.total.closed += 1

            # Streak Серия (последовательность выигрышей/проигрышей)
            for wlname in ["won", "lost"]:
                wl = res[wlname]

                trades.streak[wlname].current *= wl
                trades.streak[wlname].current += wl

                ls = trades.streak[wlname].longest or 0
                trades.streak[wlname].longest = max(ls, trades.streak[wlname].current)

            trpnl = trades.pnl
            trpnl.gross.total += trade.pnl
            trpnl.gross.average = trades.pnl.gross.total / trades.total.closed
            trpnl.net.total += trade.pnlcomm
            trpnl.net.average = trades.pnl.net.total / trades.total.closed

            # Won/Lost statistics Статистика выигрышей/проигрышей
            for wlname in ["won", "lost"]:
                wl = res[wlname]
                trwl = trades[wlname]

                trwl.total += (
                    wl  # won.total / lost.total Всего выигрышей / Всего проигрышей
                )

                trwlpnl = trwl.pnl
                pnlcomm = trade.pnlcomm * wl

                trwlpnl.total += pnlcomm
                trwlpnl.average = trwlpnl.total / (trwl.total or 1.0)

                wm = trwlpnl.max or 0.0
                func = max if wlname == "won" else min
                trwlpnl.max = func(wm, pnlcomm)

            # Long/Short statistics Статистика длинных/коротких позиций
            for tname in ["long", "short"]:
                trls = trades[tname]
                ls = res["t" + tname]

                trls.total += ls  # long.total / short.total Всего длинных / Всего коротких позиций
                trls.pnl.total += trade.pnlcomm * ls
                trls.pnl.average = trls.pnl.total / (trls.total or 1.0)

                for wlname in ["won", "lost"]:
                    wl = res[wlname]
                    pnlcomm = trade.pnlcomm * wl * ls

                    trls[wlname] += (
                        wl * ls
                    )  # long.won / short.won Выигрышные длинные / Выигрышные короткие позиции

                    trls.pnl[wlname].total += pnlcomm
                    trls.pnl[wlname].average = trls.pnl[wlname].total / (
                        trls[wlname] or 1.0
                    )

                    wm = trls.pnl[wlname].max or 0.0
                    func = max if wlname == "won" else min
                    trls.pnl[wlname].max = func(wm, pnlcomm)

            # Length Длительность (продолжительность) сделки
            trades.len.total += trade.barlen
            trades.len.average = trades.len.total / trades.total.closed
            ml = trades.len.max or 0
            trades.len.max = max(ml, trade.barlen)

            ml = trades.len.min or MAXINT
            trades.len.min = min(ml, trade.barlen)

            # Length Won/Lost Длительность выигрышных/проигрышных сделок
            for wlname in ["won", "lost"]:
                trwl = trades.len[wlname]
                wl = res[wlname]

                trwl.total += trade.barlen * wl
                trwl.average = trwl.total / (trades[wlname].total or 1.0)

                m = trwl.max or 0
                trwl.max = max(m, trade.barlen * wl)
                if trade.barlen * wl:
                    m = trwl.min or MAXINT
                    trwl.min = min(m, trade.barlen * wl)

            # Length Long/Short Длительность длинных/коротких позиций
            for lsname in ["long", "short"]:
                trls = trades.len[
                    lsname
                ]  # trades.len.long Длительность сделок в длинных позициях
                ls = res[
                    "t" + lsname
                ]  # tlong/tshort Всего длинных/Всего коротких (сделок)

                barlen = trade.barlen * ls

                trls.total += barlen  # trades.len.long.total Общая длительность сделок в длинных позициях
                total_ls = trades[
                    lsname
                ].total  # trades.long.total Общее количество длинных сделок
                trls.average = trls.total / (total_ls or 1.0)

                # max/min максимум/минимум
                m = trls.max or 0
                trls.max = max(m, barlen)
                m = trls.min or MAXINT
                trls.min = min(m, barlen or m)

                for wlname in ["won", "lost"]:
                    wl = res[wlname]  # won/lost выигрыш/проигрыш

                    barlen2 = trade.barlen * ls * wl

                    trls_wl = trls[
                        wlname
                    ]  # trades.len.long.won длительность выигрышных длинных сделок
                    trls_wl.total += barlen2  # trades.len.long.won.total общая длительность выигрышных длинных сделок

                    trls_wl.average = trls_wl.total / (trades[lsname][wlname] or 1.0)

                    # max/min
                    m = trls_wl.max or 0
                    trls_wl.max = max(m, barlen2)
                    m = trls_wl.min or MAXINT
                    trls_wl.min = min(m, barlen2 or m)
