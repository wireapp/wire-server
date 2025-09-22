CREATE TABLE public.user_group_channel (
    user_group_id uuid NOT NULL,
    conv_id uuid NOT NULL,
    PRIMARY KEY (user_group_id, conv_id)
);

ALTER TABLE ONLY public.user_group_channel
    ADD CONSTRAINT fk_user_group_channel FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;
