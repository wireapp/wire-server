CREATE TABLE public.user_group_channels (
    user_group_id uuid NOT NULL,
    channel_ids json[] NOT NULL,
    PRIMARY KEY (user_group_id)
);

ALTER TABLE ONLY public.user_group_channels
    ADD CONSTRAINT fk_user_group_channels FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;
